(in-package #:supportcentre)

(defparameter *file-directory* "/tmp/supportfiles/")

(defclass file (storable linkable)
  ((hash :initarg :hash :reader hash-of)
   (name :initarg :name :reader name-of)
   (mime-type :initarg :mime-type :reader mime-type-of)
   (note :initarg :note :accessor note-of)))

(defmethod storage-dependencies ((type (eql 'file)))
  '((note-of note)))

(defmethod print-object ((file file) stream)
  (print-unreadable-object (file stream :type t :identity t)
    (format stream
            "~a ~a ~a"
            (subseq (hash-of file) 0 6)
            (name-of file)
            (mime-type-of file))))

(defmethod serialize nconc ((file file))
  (list :hash (hash-of file)
        :name (name-of file)
        :mime-type (mime-type-of file)
        :note (storage-id (note-of file))))

(defmethod linkable-href ((file file))
  (restas:genurl 'file :id (storage-id file) :name (name-of file)))

(defun stored-path-of (file)
  (in-file-directory (hash-subpath (hash-of file))))

(defun hash-file (path)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-file :sha1 path)))

(defun hash-subpath (hash)
  (let ((dir (concatenate 'string (subseq hash 0 2) "/"))
        (name (subseq hash 2)))
    (merge-pathnames name dir)))

(defun in-file-directory (path)
  (merge-pathnames path *file-directory*))

(defun store-file (path)
  (let* ((hash (hash-file path))
         (new-path (in-file-directory (hash-subpath hash))))
    (ensure-directories-exist new-path)
    (rename-file path new-path)
    hash))

(defun handle-upload (tuple)
  (when-let (path (and tuple (listp tuple) (probe-file (first tuple))))
    (make-instance 'file
                   :hash (store-file path)
                   :name (second tuple)
                   :mime-type (third tuple))))
