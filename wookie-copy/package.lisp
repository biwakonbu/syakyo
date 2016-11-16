(defpackage :wookie-copy
  (:use :cl :asdf)
  (:export #:*hide-version*

           #:clear-routes
           #:clear-route
           #:defroute

           #:clear-hooks
           #:add-hook
           #:remove-hook

           #:request
           #:request-method
           #:request-resource
           #:request-http
           #:request-plugin-data
           #:response
           #:response-headers
           #:with-chunking
           #:send-response
           #:start-response
           #:finish-response

           #:acceptor
           #:start-server))
