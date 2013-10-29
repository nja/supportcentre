(in-package #:supportcentre)

(defclass view () ())

(defgeneric render-route-data (view data route))

(defgeneric finalize-page (view data rendered))

(defmethod restas:render-object ((view view) (data (eql nil)))
  (declare (ignore view data))
  (setf (hunchentoot:return-code*) hunchentoot:+HTTP-NOT-FOUND+)
  (hunchentoot:abort-request-handler))

(defmethod restas:render-object ((view view) (data list))
  (let* ((route (restas:route-symbol restas:*route*))
         (rendered (render-route-data view data route)))
    (finalize-page view data rendered)))

;;; Template view

(defclass view-tmpl (view) ())

(defmethod render-route-data ((view view-tmpl) data route)
  (let ((template (find-symbol (symbol-name route) '#:view)))
    (if template
        (funcall template data)
        (format nil "No template for route ~A" route))))

(defmethod finalize-page ((view view-tmpl) data rendered)
  (declare (ignore view))
  (view:main (list* :body rendered data)))

(defmethod closure-template:fetch-property ((timed timed) (key (eql :CHANGE-TIME)))
  (pretty-time (change-time-of timed)))

;;; JSON view

(defclass view-json (view) ())

(defmethod jsown:to-json :around ((list list))
  "Convert plists as objects"
  (if (and (symbolp (car list)) (not (eq :OBJ (car list))))
      (jsown:to-json
       (list* :OBJ (loop for (key value) on list by #'cddr
                         collect (cons (string-downcase (symbol-name key))
                                       value))))
      (call-next-method)))

(defmethod render-route-data ((view view-json) data route)
  (when data
    (jsown:to-json data)))

(defmethod finalize-page ((view view-json) data rendered)
  rendered)

(defmethod jsown:to-json ((area area))
  (jsown:to-json
   (jsown:new-js
     ("id" (storage-id area))
     ("url" (linkable-href area))
     ("change-time" (change-time-of area))
     ("name" (name-of area))
     ("owner" (user-of area)))))

(defmethod jsown:to-json ((user user))
  (jsown:to-json
   (jsown:new-js
     ("id" (storage-id user))
     ("url" (linkable-href user))
     ("name" (name-of user))
     ("realname" (realname-of user)))))

(defmethod jsown:to-json ((timestamp timestamp))
  (jsown:to-json (iso-time timestamp)))

(defmethod jsown:to-json ((issue issue))
  (jsown:to-json
   (jsown:new-js
     ("id" (storage-id issue))
     ("url" (linkable-href issue))
     ("change-time" (change-time-of issue))
     ("creator" (user-of issue))
     ("area" (area-of issue))
     ("subject" (subject-of issue)))))

(defmethod jsown:to-json ((note note))
  (jsown:to-json
   (jsown:new-js
     ("id" (storage-id note))
     ("url" (linkable-href note))
     ("change-time" (change-time-of note))
     ("issue-url" (linkable-href (issue-of note)))
     ("user" (user-of note))
     ("text" (text-of note))
     ("files" (slot-value note 'files)))))

(defmethod jsown:to-json ((file file))
  (jsown:to-json
   (jsown:new-js
     ("hash" (hash-of file))
     ("url" (linkable-href file))
     ("name" (name-of file))
     ("mime-type" (mime-type-of file))
     ("note-url" (linkable-href (note-of file))))))
