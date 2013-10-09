(in-package #:supportcentre)

(defgeneric linkable-href (thing))

(defclass linkable ()
  ((href :accessor href)))

(defmethod linkable-href ((thing storable))
  (restas:genurl (storage-type thing) :id (storage-id thing)))

(defmethod (setf storage-id) :after (id (thing linkable))
  (setf (href thing) (linkable-href thing)))
