(in-package #:supportcentre)

(defclass storable ()
  ((id :initarg :id :accessor storage-id)))

(defgeneric serialize (thing)
  (:method ((thing list))
    (with-output-to-string (out)
      (prin1 thing out))))

(defgeneric deserialize (type string)
  (:method (type (string (eql nil)))
    nil)
  (:method ((type (eql 'list)) string)
    (values (safe-read string)))
  (:method ((type symbol) (string string))
    (let ((properties (safe-read string)))
      (apply #'make-instance type properties))))

(defgeneric storage-type (thing)
  (:method ((thing storable))
    (type-of thing)))

(defgeneric storage-key (thing)
  (:method ((thing storable))
    (make-key (storage-type thing) (storage-id thing))))

(defgeneric storage-create (thing)
  (:method ((thing storable))
    (let ((serialized (serialize thing))
          (key (storage-key thing)))
      (red:setnx key serialized))))

(defmethod storage-create :before (storable)
  (let ((id (red:incr (next-id-key storable))))
    (setf (storage-id storable) id)))

(defgeneric storage-read (type id)
  (:method ((type symbol) id)
    (let* ((key (make-key type id))
           (serialized (red:get key)))
      (deserialize type serialized))))

(defgeneric storage-update (storable))

(defun next-id-key (storable)
  (make-key "next-id" (storage-type storable)))

(defun next-id (storable)
  (red:incr (next-id-key storable)))

(defun make-key (prefix suffix)
  (format nil "~A:~A" prefix suffix))
