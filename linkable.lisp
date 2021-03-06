(in-package #:supportcentre)

(defgeneric linkable-href (thing))

(defclass linkable ()
  ((href :accessor href)))

(defmethod linkable-href ((thing storable))
  (values
   (restas:genurl (storage-type thing) :id (storage-id thing))
   (format nil "~a: ~a" (type-of thing) (storage-id thing))))

(defgeneric set-href (thing))

(defmethod set-href (thing))

(defmethod set-href ((linkable linkable))
  (setf (href linkable) (linkable-href linkable)))

(defmethod storage-read-dependencies :after (type things)
  (dolist (thing things)
    (set-href thing)))
