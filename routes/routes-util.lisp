(in-package #:supportcentre)

(defmacro with-storage (&body body)
  `(redis:with-persistent-connection ()
     (with-read-cache
       ,@body)))

(defun make-links (&rest linkables)
  (remove-if-not #'identity
                 (mapcar (lambda (thing)
                           (if (listp thing)
                               thing
                               (multiple-value-bind (href text) (linkable-href thing)
                                 (list :href href :text text))))
                         linkables)))

(defun get-int-parameter (name)
  (let ((value (get-parameter name)))
    (when value
      (parse-integer value :junk-allowed t))))

(defun get-page ()
  (or (get-int-parameter "page")
      (when-let (note (get-int-parameter "note"))
        (1+ (truncate (1- note) *page-size*)))
      :last))

(defun make-page-links (numbers current route-symbol &rest args)
  (multiple-value-bind (min max) (apply #'minmax numbers)
    (when (eq current :last)
      (setf current max))
    (unless (= min max current)
     (flet ((link (n &optional text)
              (list :href (if (<= min n max)
                              (apply #'restas:genurl route-symbol :page n args)
                              "")
                    :text (if (= n current)
                              (format nil "*~a*" (or text n))
                              (or text n)))))
       (nconc (list (link (1+ current) "<<"))
              (mapcar #'link numbers)
              (list (link (1- current) ">>")))))))

(defun home ()
  (list :href (restas:genurl 'area-list) :text "Home"))

(defun login/out ()
  (if (get-user)
      (list :href (restas:genurl 'logout) :text "Log out")
      (list :href (restas:genurl 'login) :text "Log in")))

(defun must-be-logged-in ()
  (unless (get-user)
    (restas:redirect 'login :forward (url-encode (hunchentoot:request-uri*)))))

(defun memberp (thing set)
  (storage-set-member-p thing set (get-user-id)))

(defun must-be-member (thing set)
  (unless (memberp thing set)
    (forbidden)))

(defun forbidden ()
  (setf (hunchentoot:return-code hunchentoot:*reply*)
        hunchentoot:+http-forbidden+)
  (hunchentoot:abort-request-handler))
