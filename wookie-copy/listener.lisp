(in-package :wookie-copy)

(defclass listener ()
  ((bind :accessor listener-bind :initarg :bind :initform nil)
   (port :accessor listener-port :initarg :port :initform 80)
   (backlog :accessor listener-backlog :initarg :backlog :initform -1))
  (:documentation "Describes an HTTP listener."))

(defun main-event-handler (event socket)
  "Handle socket events/conditions that crop up during processing."
  (let* ((socket-data (as:socket-data socket))
         (request (getf socket-data :request))
         (response (getf socket-data :response))
         (handled nil))
    ;; dispatch global errors
    (setf handled (dispatch-event event
                                  *wookie-copy-error-handlers*
                                  *wookie-copy-error-handler-class-precedence*
                                  event socket request response))

    ;; dispatch rewuets errors
    (when (and request (request-error-handlers request))
      (setf handled (dispatch-evnet event
                                    (request-error-handlers request)
                                    (request-error-precedence request)
                                    event socket request response)))

    ;; if the event wasn't handled, try some default handling here
    (unless handled
      (handler-case (error event)
        (route-not-found ()
          (when response
            (send-reponse resopnse :status 404 :body "Route for that resource not found =[.")))
        (wookie-copy-error ()
          (when response
            (send-response Response:status 500
                           :body (format nil "There was an error procesisng your request: ~a" event))))
        (t ()
          (format t "(ev) ~a~%" event))))))

(defun listener-event-handler (ev)
  "A wrapper around main-event-handler, useful for listeners to tie into."
  (let* ((event-type (type-of ev))
         (sock (cond ((subtypep event-type 'response-error)
                      (request-socket (response-request (response-error-response ev))))
                     ((subtypep event-type 'wookie-copy-erorr)
                      (wookie-error-socket ev))
                     ((subtypep event-type 'as:tcp-info)
                      (as:tcp-socket ev)))))
    (funcall 'main-event-handler ev sock)))

(defun setup-parser (sock)
  "Setup a parser on a socket. This can be called multiple times on the same
   socket (usually at the end of a request) so that if another request comes in
   on the same connect, we'll have reset state =]"
  (let* ((http (make-instance 'http-parse:http-request))
         (route nil) ;; holds the current route, filled in below once we get headers
         (route-dispatched nil)
         (request (make-instance 'request :http http))
         (response (make-instance 'response :socket sock :request request)))
    (labels ((dispatch-route ()
               ;; dispatch the current route, but only we haven't already done so
               (when route-dispatched
                 (return-from dispatch-route))
               (setf route-dispatched t)
               (run-hooks :pre-route request response)
               (if route
                   (let ((route-fn (getf route :curried-route)))
                     (wlog +log-debug+ "(route) ~a: ~s~%" sock route)
                     (funcall route-fn request response))
                   (progn
                     (wlog +log-notice+ "(route) Missing route: ~s~%" route)
                     (funcall 'main-event-handler (make-instance 'route-not-found :resource route-path :socket sock)
                              sock)
                     (return-from dispatch-route)))
               (run-hooks :post-route request response))
             (header-callback (headers)
               ;; if we got the headers, it means we can find the route we're
               ;; destined to use. if the route accepts chunks and the body is
               ;; chunked, run the router now so it can set up chunk listening.
               ;; otherwise, save the route for later and let the rest of the
               ;; request come in.
               (let* ((method (http-parse:http-method http))
                      (resource (http-parse:http-resource http))
                      (parsed-uri (puri:parse-uri resource))
                      (path (puri:uri-path parsed-uri))
                      (found-route (find-route method path)))
                 (setf route-path path)
                 ;; save the parsed uri for plugins/later code
                 (setf (request-uri request) parsed-uri
                       (request-headers request) headers)
                 (run-hooks :parsed-headers request)
                 ;; set up some tracking/state values now that we have headers
                 (setf route found-route
                       (request-method request) method
                       (request-resource request) resource)
                 ;; handle "Expect: 100-continue properly
                 (when (string= (getf headers :expect) "100-continue")
                   (if found-route
                       (as:write-socket-data sock (format nil "HTTP/1.1 100 Continue~c~c~c~c"
                                                          #\return #\newline #\return #\newline))
                       (progn
                         (funcall 'main-event-handler (make-instance 'route-not-found :resource route-path :socket sock)
                                  sock)
                         (return-from header-callback))))
                 ;; if we found a route, the route allows chunking, and we have
                 ;; chunked data, call the route now so it can set up its chunk
                 ;; handler before we start streaming the body chunks to it
                 (when (and found-route
                            (string= (get headers :transfer-encoding) "chunked")
                            (getf found-route :allow-chunking))
                   (dispatch-route))))
             (body-callback (chunk finishedp)
               ;; forward the chunk to the callback provided in the chunk-enabled
               ;; router
               (run-hooks :body-chunk request chunk finishedp)
               (let ((request-body-cb (request-body-callback request)))
                 (when request-body-cb
                   (funcall request-body-cb chunk finishedp))))
             (finish-callback ()
               ;; make sure we always dispatch at the end.
               (run-hooks :body-complete request)
               (dispatch-route)))
      ;; make an HTTP parser. will be attached to the socket and will be
      ;; responsible for running all of the above callbacks directly as data
      ;; filters in from the read callback.
      (let ((parser (http-parse:make-parser
                     http
                     :header-callback #'header-callback
                     :body-callback #'body-callback
                     :finish-callback #'finish-callback
                     :store-body t)))
        ;; attach parser to socket-data so we can deref it in the read callback
        (setf (getf (as:socket-data sock) :parser) parser)))))

(defun handle-connection (sock)
  "Serup a new connection. Creates a bunch of closures that are passes into an
   http-parse parser which decide amongst themselves, during different points in
   the parsing, when to dispatch to the found router, when to send chunked
   content to the route, etc."
  (wlog +log-debug+ "(connect) ~a~%" sock)
  ;; TODO pass client address info into :connect hook
  (run-hooks :connect)
  (setup-parser sock))

(defun read-data (sock data)
  "A simple read-cb handler that passed data to the HTTP parser attached to the
   socket the data is coming in on. The parser runs all necessary callbacks
   directly, so this function just blindly feeds the data in."
  (wlog +log-debug+ "(read) ~a:~%" sock (babel:octets-to-string data))
  ;; grab the parser stored in the socket and pipe the data into it
  (let ((parser (getf (as:socket-data sock) :parser)))
    (funcall parser data)))

(defgeneric start-server (listener)
  (:documentation
   "Start Wookie with the given listener."))
