(in-package #:supportcentre)

(defgeneric serialize (thing) (:method-combination nconc))
(defgeneric deserialize (type string))
(defgeneric storage-type (thing))
(defgeneric storage-key (thing))
(defgeneric storage-create (thing))
(defgeneric storage-read (type id))
(defgeneric storage-read-many (type ids))
(defgeneric storage-read-set (type set))
(defgeneric storage-read-backrefs (type thing &key start stop))
(defgeneric storage-update (thing))
(defgeneric storage-lookup (type lookup value))
(defgeneric storage-exists-p (type id))
(defgeneric storage-read-dependencies (type things))
(defgeneric storage-dependencies (type))

(defclass storable ()
  ((id :initarg :id :accessor storage-id)
   (sets :initarg :sets :initform (list :all) :accessor storage-sets)))

(defmethod print-object ((thing storable) stream)
  (print-unreadable-object (thing stream :type t :identity t)
    (princ (slot-value-default thing 'id #\?) stream)))

(defmethod serialize :around (thing)
  (prin1-to-string (call-next-method)))

(defmethod serialize nconc ((list list))
  list)

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
  (let ((thing (apply #'create type id (redis:with-pipelining
                                         (red:get (thing-key type id))
                                         (red:get (sets-key type id))))))
    (when thing
      (storage-read-dependencies type (list thing)))
    thing))

(defun create (type id thing-string sets-string)
  (when (and thing-string sets-string)
    (let ((thing (deserialize type thing-string))
          (sets (deserialize 'list sets-string)))
      (setf (storage-id thing) id
            (storage-sets thing) sets)
      (cache-add thing))))

(defun read-things-data (type ids)
  (when-let (data&sets (redis:with-pipelining
                         (dolist (id ids)
                           (red:get (thing-key type id))
                           (red:get (sets-key type id)))))
    (loop for id in ids
          for (data sets) on data&sets by #'cddr
          collect (list id data sets))))

(defun uncached-ids (type ids)
  (if *read-cache*
      (loop with cache = *read-cache*
            with key = (cons type nil)
            for id in ids
            for present-p = (progn (setf (cdr key) id)
                                   (gethash key cache))
            unless present-p
              do (setf (gethash key cache) :pending)
            unless present-p collect id)
      ids))

(defmethod storage-read-many ((type symbol) ids)
  (with-read-cache
    (let* ((uncached-ids (uncached-ids type ids))
           (data (read-things-data type uncached-ids))
           (created (mapcar (lambda (data) (apply #'create type data))
                            data)))
      (storage-read-dependencies type created)
      (mapcar #'(lambda (id) (cache-read type id)) ids))))

(defmethod storage-read-set ((type symbol) set)
  (storage-read-many type (read-id-set (set-key type set))))

(defmethod storage-lookup ((type symbol) (lookup symbol) value)
  (when-let (id (red:get (lookup-key type lookup value)))
    (cache-read type id)))

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

(defmethod storage-id ((string string))
  "Hack for when an id has not yet been replaced by its object."
  string)

(defmethod storage-id ((integer integer))
  "Hack for when an id has not yet been replaced by its object."
  integer)

(defmethod storage-dependencies ((type t)))

(defmethod storage-dependencies ((thing storable))
  (mapcar #'(lambda (dependency) (funcall (car dependency) thing))
          (storage-dependencies (storage-type thing))))

(defmethod storage-read-dependencies ((type symbol) things)
  (with-read-cache
    (dolist (dependency (storage-dependencies type) things)
      (destructuring-bind (accessor dep-type) dependency
        (let* ((dep-ids (mapcar accessor things))
               (dep-things (storage-read-many dep-type dep-ids))
               (setter (fdefinition `(setf ,accessor))))
          (mapc (lambda (thing dep)
                  (funcall setter dep thing))
                things
                dep-things))))))

(defmethod storage-read-backrefs ((type symbol) (thing storable) &key (start 0) (stop -1))
  (let* ((storage-key (backref-key thing type))
         (ids (read-id-list storage-key :start start :stop stop)))
    (storage-read-many type ids)))

(defmethod storage-create :after ((thing storable))
  (dolist (dep (storage-dependencies thing))
    (add-backref dep thing)))

(defmacro with-read-cache (&body body)
  `(let ((*read-cache* (or *read-cache* (make-hash-table :test 'equal))))
     ,@body))

(defvar *read-cache* nil)

(defun cache-read (type id)
  (or (when *read-cache* (gethash (cons type id) *read-cache*))
      (storage-read type id)))

(defun cache-add (thing)
  (if *read-cache*
      (setf (gethash (cons (storage-type thing) (storage-id thing))
                     *read-cache*)
            thing)
      thing))

(defun add-backref (dep thing)
  (red:rpush (backref-key dep (storage-type thing)) (storage-id thing)))

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

(defun backref-key (dep type)
  (make-key (thing-key (storage-type dep) (storage-id dep)) :BACKREF type))

(defun make-key (&rest parts)
  (format nil "~{~a~^:~}" parts))

(defun read-id-set (key)
  (mapcar #'parse-integer (red:smembers key)))

(defun read-id-list (key &key (start 0) (stop -1))
  (mapcar #'parse-integer (red:lrange key start stop)))
