(defpackage :wookie-copy
  (:use :cl :asdf)
  (:export #:clear-routes
           #:clear-route
           #:defroute

           #:clear-hooks
           #:add-hook

           #:request
           #:request-method
           #:request-resource
           #:request-http
           #:request-plugin-data
           #:response
           #:response-headers
           #:send-response
           #:start-response
           #:finish-response

           #:acceptor
           #:start-server))
