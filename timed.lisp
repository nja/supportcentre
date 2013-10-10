(in-package #:supportcentre)

(defclass timed ()
  ((creation-time :initarg :creation-time :accessor creation-time-of)
   (change-time :initarg :change-time :accessor change-time-of)))

(defmethod storage-create :before ((thing timed))
  (let ((now (now)))
    (setf (creation-time-of thing) now
          (change-time-of thing) now)))

(defmethod storage-update :before ((thing timed))
  (setf (change-time-of thing) (now)))

(defmethod serialize nconc ((timed timed))
  (list :creation-time (creation-time-of timed)
        :change-time (change-time-of timed)))
