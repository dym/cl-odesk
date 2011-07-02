=========================
What is cl-odesk?
=========================
This is cl-odesk, an implementation of the oDesk API in Common Lisp.

Spec URI: http://developers.odesk.com

=========================
Example Usage
=========================

If you already installed cl-odesk you can load it by::

 (require :odesk)

Before doing some API calls first you need to define connection. You will need PUBLIC_KEY, PRIVATE_KEY and TOKEN. The last one you can get only by authenticating yourself in your favourite web browser. Please read API Documentation at http://developers.odesk.com

You can create connection by instantiating API class::

 (defparameter *test-connection*
   (make-instance 'odesk:api-json
                  :public-key "PUBLIC"
                  :secret-key "SECRET"
                  :api-token "TOKEN"))

Here is macro that can ease your life a lot::

 (with-odesk (:connection con
              :public-key "PUBLIC"
              :secret-key "SECRET"
              :api-token "TOKEN")
   (print con))

And another macro that can help too::

 (connect-odesk (:public-key "PUBLIC"
                 :secret-key "SECRET"
                 :api-token "TOKEN"))

Now let's do request to get info about user.
If you went first way and created variable *test-connection*::

 (odesk:hr/get-user :connection *test-connection*)

If you go the second way and will use with-odesk macro. Here you have two options. The first one::

 (odesk:with-odesk
     (:connection my-con
      :public-key "PUBLIC"
      :secret-key "SECRET"
      :api-token "TOKEN")
   (odesk:hr/get-user :connection my-con))

The second::

 (odesk:with-odesk
     (:public-key "PUBLIC"
      :secret-key "SECRET"
      :api-token "TOKEN")
   (odesk:hr/get-user))

Third way is for those who want to create connection globally and then use it in the code with-out the need to use with-odesk macro::

 (odesk:connect-odesk (:public-key "PUBLIC"
                       :secret-key "SECRET"
                       :api-token "TOKEN"))
 (odesk:hr/get-user)
