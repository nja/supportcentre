(in-package #:supportcentre)

(defclass designer () ())

(defgeneric render-route-data (designer data route))

(defmethod render-route-data ((designer designer) data route)
  (let ((template (find-symbol (symbol-name route) '#:view)))
    (if template
        (funcall template data)
        (format nil "No template for route ~A" route))))

(defmethod restas:render-object ((designer designer) (data (eql nil)))
  (setf (hunchentoot:return-code*) hunchentoot:+HTTP-NOT-FOUND+)
  (hunchentoot:abort-request-handler))

(defmethod restas:render-object ((designer designer) (data list))
  (let ((route (restas:route-symbol restas:*route*)))
    (finalize-page designer
                   (list :body (render-route-data designer data route)
                         :title (getf data :title)))))

(defgeneric finalize-page (designer data))

(defmethod finalize-page ((desiger designer) data)
  (view:main data))
