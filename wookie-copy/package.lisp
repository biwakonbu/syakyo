(defpackage :wookie-copy
  (:use :cl :asdf)
  (:export #:clear-routes
           #:clear-route
           #:defroute
           #:acceptor
           #:start-server
           #:send-reply))
