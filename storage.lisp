(in-package #:supportcentre)

(defclass storable ()
  ((id :initarg :id :accessor storage-id)
   (sets :initarg :sets :initform nil :accessor storage-sets)))

(defgeneric serialize (thing)
  (:method ((thing list))
    (with-output-to-string (out)
      (prin1 thing out))))

(defgeneric deserialize (type string)
  (:method (type (string (eql nil)))
    nil)
  (:method ((type (eql 'list)) string)
    (when string
      (values (safe-read string))))
  (:method ((type symbol) (string string))
    (let ((properties (safe-read string)))
      (apply #'make-instance type properties))))

(defgeneric storage-type (thing)
  (:method ((thing storable))
    (type-of thing)))

(defgeneric storage-key (thing)
  (:method ((thing storable))
    (thing-key (storage-type thing) (storage-id thing))))

(defgeneric storage-create (thing)
  (:method ((thing storable))
    (let ((id (red:incr (next-id-key thing))))
      (setf (storage-id thing) id)
      (storage-update thing))))

(defgeneric storage-read (type id)
  (:method ((type symbol) id)
    (apply #'create type id (redis:with-pipelining
                              (red:get (thing-key type id))
                              (red:get (sets-key type id))))))

(defun create (type id thing-string sets-string)
  (let ((thing (deserialize type thing-string))
        (sets (deserialize 'list sets-string)))
    (setf (storage-id thing) id
          (storage-sets thing) sets)
    thing))

(defgeneric storage-read-set (type set)
  (:method ((type symbol) set)
    (let* ((ids (red:smembers (set-key type set)))
           (data (redis:with-pipelining
                   (dolist (id ids)
                     (red:get (thing-key type id))
                     (red:get (sets-key type id))))))
      (loop for id in ids
            for (thing-string set-string) on data by #'cddr
            collect (create type id thing-string set-string)))))

(defgeneric storage-update (thing)
  (:method ((thing storable))
    (let ((serialized (serialize thing))
          (key (storage-key thing))
          (sets-key (sets-key (storage-type thing) (storage-id thing))))
      (prog1 (red:set key serialized)
        (red:set sets-key (serialize (storage-sets thing)))
        (dolist (set (storage-sets thing))
          (red:sadd (set-key (storage-type thing) set) (storage-id thing)))))))

(defun next-id-key (storable)
  (make-key 'next-id (storage-type storable)))

(defun set-key (type set)
  (make-key type 'set set))

(defun sets-key (type id)
  (make-key type id 'sets))

(defun next-id (storable)
  (red:incr (next-id-key storable)))

(defun thing-key (type id)
  (make-key type id))

(defun make-key (prefix suffix &rest more)
  (if more
      (make-key prefix (apply #'make-key suffix more))
      (format nil "~A:~A" prefix suffix)))
