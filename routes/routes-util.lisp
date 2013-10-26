(in-package #:supportcentre)

(defmacro with-storage (&body body)
  `(redis:with-persistent-connection ()
     (with-read-cache
       ,@body)))

(defun make-links (&rest linkables)
  (mapcar (lambda (thing)
            (if (listp thing)
                thing
                (multiple-value-bind (href text) (linkable-href thing)
                  (list :href href :text text))))
          linkables))

(defun home ()
  (list :href (restas:genurl 'area-list) :text "Home"))

(defun login/out ()
  (if (get-user)
      (list :href (restas:genurl 'logout) :text "Log out")
      (list :href (restas:genurl 'login) :text "Log in")))

(defun must-be-logged-in ()
  (unless (get-user)
    (restas:redirect 'login :forward (hunchentoot:request-uri*))))

(defun memberp (thing set)
  (let ((member-ids (read-id-set (thing-set-key thing set)))
        (user-id (get-user-id)))
    (member user-id member-ids)))

(defun must-be-member (thing set)
  (unless (memberp thing set)
    (forbidden)))

(defun forbidden ()
  (setf (hunchentoot:return-code hunchentoot:*reply*)
        hunchentoot:+http-forbidden+)
  (hunchentoot:abort-request-handler))
