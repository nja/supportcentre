(in-package #:supportcentre)

(defgeneric serialize (thing))
(defgeneric deserialize (type string))
(defgeneric storage-type (thing))
(defgeneric storage-key (thing))
(defgeneric storage-create (thing))
(defgeneric storage-read (type id))
(defgeneric storage-read-many (type ids))
(defgeneric storage-read-set (type set))
(defgeneric storage-update (thing))
(defgeneric storage-lookup (type lookup value))
(defgeneric storage-exists-p (type id))

(defclass storable ()
  ((id :initarg :id :accessor storage-id)
   (sets :initarg :sets :initform (list :all) :accessor storage-sets)))

(defmethod serialize ((thing list))
  (prin1-to-string thing))

(defmethod deserialize (type (string (eql nil)))
  nil)

(defmethod deserialize ((type (eql 'list)) string)
  (when string
    (values (safe-read string))))

(defmethod deserialize ((type symbol) (string string))
  (let ((properties (safe-read string)))
    (apply #'make-instance type properties)))

(defmethod storage-type ((thing storable))
  (type-of thing))

(defmethod storage-key ((thing storable))
  (thing-key (storage-type thing) (storage-id thing)))

(defmethod storage-create ((thing storable))
  (let ((id (red:incr (next-id-key thing))))
    (setf (storage-id thing) id)
    (values id (storage-update thing))))

(defmethod storage-read ((type symbol) id)
  (apply #'create type id (redis:with-pipelining
                            (red:get (thing-key type id))
                            (red:get (sets-key type id)))))

(defun create (type id thing-string sets-string)
  (when (and thing-string sets-string)
    (let ((thing (deserialize type thing-string))
          (sets (deserialize 'list sets-string)))
      (setf (storage-id thing) id
            (storage-sets thing) sets)
      thing)))

(defmethod storage-read-many ((type symbol) ids)
  (let ((data (redis:with-pipelining
                (dolist (id ids)
                  (red:get (thing-key type id))
                  (red:get (sets-key type id))))))
    (loop for id in ids
          for (thing-string set-string) on data by #'cddr
          collect (create type id thing-string set-string))))

(defmethod storage-read-set ((type symbol) set)
  (storage-read-many type (red:smembers (set-key type set))))

(defmethod storage-lookup ((type symbol) (lookup symbol) value)
  (when-let (id (red:get (lookup-key type lookup value)))
    (storage-read type id)))

(defmethod storage-update ((thing storable))
  (let ((serialized (serialize thing))
        (key (storage-key thing))
        (sets-key (sets-key (storage-type thing) (storage-id thing))))
    (redis:with-pipelining
      (red:set key serialized)
      (red:set sets-key (serialize (storage-sets thing)))
      (dolist (set (storage-sets thing))
        (red:sadd (set-key (storage-type thing) set) (storage-id thing))))))

(defmethod storage-exists-p ((type symbol) id)
  (red:exists (thing-key type id)))

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

(defun lookup-key (type lookup value)
  (make-key type lookup value))

(defun make-key (prefix suffix &rest more)
  (if more
      (make-key prefix (apply #'make-key suffix more))
      (format nil "~A:~A" prefix suffix)))
