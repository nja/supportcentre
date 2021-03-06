(in-package #:supportcentre)

(defgeneric serialize (thing) (:method-combination nconc))
(defgeneric deserialize (type string))
(defgeneric storage-type (thing))
(defgeneric storage-key (thing))
(defgeneric storage-create (thing))
(defgeneric storage-read (type id))
(defgeneric storage-read-set (type owner set))
(defgeneric storage-read-backrefs (type thing &key page page-size))
(defgeneric storage-update (thing))
(defgeneric storage-lookup (type lookup value))
(defgeneric storage-exists-p (type id))
(defgeneric storage-read-dependencies (type things))
(defgeneric storage-dependencies (type))
(defgeneric storage-set-add (owner set addee))
(defgeneric storage-set-remove (owner set removee))
(defgeneric storage-set-member-p (owner set testee))

(defclass storable (timed)
  ((id :initarg :id :accessor storage-id)
   (sets :initarg :sets :initform (list :all) :accessor storage-sets)
   (index :initarg nil :accessor index-of)))

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
    (values thing (storage-update thing))))

(defvar *read-cache* nil)

(defmacro with-read-cache (&body body)
  `(if *read-cache*
       (progn ,@body)
       (let ((*read-cache* (make-hash-table :test 'equal)))
         ,@body)))

(defmethod storage-read ((type symbol) id)
  (let ((thing (apply #'create type id (redis:with-pipelining
                                         (red:get (thing-key type id))
                                         (red:get (sets-key type id))))))
    (when thing
      (storage-read-dependencies type (list thing)))
    thing))

(defmethod storage-read ((type symbol) (ids list))
  (with-read-cache
    (let* ((uncached-ids (uncached-ids type ids))
           (data (read-things-data type uncached-ids))
           (created (mapcar (lambda (data) (apply #'create type data))
                            data)))
      (storage-read-dependencies type created)
      (mapcar #'(lambda (id) (cache-read type id)) ids))))

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

(defmethod storage-read-set ((type symbol) (owner symbol) set)
  (storage-read type (read-id-set (set-key owner set))))

(defmethod storage-read-set ((type symbol) (owner storable) set)
  (storage-read type (read-id-set (thing-set-key owner set))))

(defmethod storage-set-add ((owner symbol) set (addee storable))
  (red:sadd (set-key owner set) (storage-id addee)))

(defmethod storage-set-add ((owner storable) set (addee storable))
  (red:sadd (thing-set-key owner set) (storage-id addee)))

(defmethod storage-set-remove ((owner storable) set (removee storable))
  (red:srem (thing-set-key owner set) (storage-id removee)))

(defmethod storage-set-remove ((owner symbol) set (removee storable))
  (red:srem (set-key owner set) (storage-id removee)))

(defmethod storage-set-member-p ((owner symbol) set (id integer))
  (red:sismember (set-key owner set) id))

(defmethod storage-set-member-p ((owner storable) set (id integer))
  (red:sismember (thing-set-key owner set) id))

(defmethod storage-set-member-p (owner set (testee storable))
  (call-next-method owner set (storage-id testee)))

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
               (dep-things (storage-read dep-type dep-ids))
               (setter (fdefinition `(setf ,accessor))))
          (mapc (lambda (thing dep)
                  (funcall setter dep thing))
                things
                dep-things))))))

(defmethod storage-read-backrefs ((type symbol) (thing storable)
                                  &key (page :all) (page-size *page-size*))
  (let ((storage-key (backref-key thing type)))
    (multiple-value-bind (ids start) (read-id-page storage-key
                                                   :page page
                                                   :page-size page-size)
      (reverse-index-from (storage-read type ids) start))))

(defmethod storage-update :after ((thing storable))
  (dolist (dep (storage-dependencies thing))
    (update-backref dep thing)))

(defun cache-read (type id)
  (or (when *read-cache*
        (let ((key (cons type id)))
          (declare (dynamic-extent key))
          (gethash key *read-cache*)))
      (storage-read type id)))

(defun cache-add (thing)
  (if *read-cache*
      (let ((key (cons (storage-type thing) (storage-id thing))))
        (setf (gethash key *read-cache*) thing))
      thing))

(defun update-backref (dep thing)
  (red:zadd (backref-key dep (storage-type thing))
            (timestamp-to-unix (change-time-of thing))
            (storage-id thing)))

(defun next-id-key (storable)
  (make-key 'next-id (storage-type storable)))

(defun set-key (type set)
  (make-key type 'set set))

(defun thing-set-key (thing set)
  (make-key (thing-key (type-of thing) (storage-id thing)) 'set set))

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
  (let ((ids (red:zrange key start stop)))
    (mapcar #'parse-integer ids)))

(defun read-id-page (key &key (page :last) (page-size *page-size*))
  (case page
    (:all
     (values (read-id-list key) 0))
    (:last
     (let ((count (red:zcard key)))
       (multiple-value-bind (full rest) (truncate count page-size)
         (read-id-page key :page (if (zerop rest)
                                     full
                                     (1+ full))
                           :page-size page-size))))
    (t (let* ((i (1- page))
              (start (* i page-size))
              (stop (+ start (1- page-size))))
         (values (read-id-list key :start start :stop stop) start)))))
