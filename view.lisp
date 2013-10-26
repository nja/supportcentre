(in-package #:supportcentre)

(defclass view () ())

(defgeneric render-route-data (view data route))

(defmethod render-route-data ((view view) data route)
  (let ((template (find-symbol (symbol-name route) '#:view)))
    (if template
        (funcall template data)
        (format nil "No template for route ~A" route))))

(defmethod restas:render-object ((view view) (data (eql nil)))
  (declare (ignore view data))
  (setf (hunchentoot:return-code*) hunchentoot:+HTTP-NOT-FOUND+)
  (hunchentoot:abort-request-handler))

(defmethod restas:render-object ((view view) (data list))
  (let* ((route (restas:route-symbol restas:*route*))
         (body (render-route-data view data route)))
    (finalize-page view
                   (nconc (list :body body) data))))

(defgeneric finalize-page (view data))

(defmethod finalize-page ((view view) data)
  (declare (ignore view))
  (view:main data))
