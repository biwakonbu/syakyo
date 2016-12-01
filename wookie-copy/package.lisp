(defpackage :wookie-copy
  (:use :cl :wookie-copy-util)
  (:export #:*hide-version*
           #:*tmp-file-store*

           #:wookie-error
           #:wookie-error-msg
           #:wookie-error-socket
           #:add-error-handler

           #:route-error
           #:route-not-found
           #:clear-routes
           #:clear-route
           #:defroute

           #:clear-hooks
           #:add-hook
           #:remove-hook

           #:request
           #:request-socket
           #:request-method
           #:request-resource
           #:request-headers
           #:request-uri
           #:request-plugin-data
           #:request-body-collback
           #:request-http
           #:response
           #:response-headers
           #:response-finished-p
           #:with-chunking
           #:send-response
           #:start-response
           #:finish-response
           #:add-request-error-handler

           #:load-plugins

           #:listener
           #:listener-bind
           #:listener-port
           #:listener-backlog
           #:start-server

           #-(or :wookie-copy-no-ssl) #:listener-certificate
           #-(or :wookie-copy-no-ssl) #:listener-key
           #-(or :wookie-copy-no-ssl) #:listener-password))
