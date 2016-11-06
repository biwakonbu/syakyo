(defpackage :wookie-copy
  (:use :cl :asdf)
  (:export #:clear-routes
           #:clear-route
           #:defroute

           #:clear-hooks
           #:add-hook

           #:set-plugin-folder
           #:load-plugins

           #:request
           #:request-method
           #:request-resource
           #:request-http
           #:response
           #:response-headers
           #:send-response

           #:acceptor
           #:start-server))
