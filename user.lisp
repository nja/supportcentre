(in-package #:supportcentre)

(defclass user (storable linkable)
  ((name :initarg :name :initform "" :accessor name-of)
   (realname :initarg :realname :initform "" :accessor realname-of)
   (password :initarg :password :initform nil :accessor password-of)))

(defmethod print-object ((user user) stream)
  (print-unreadable-object (user stream :type t :identity t)
    (princ (slot-value-default user 'id #\?) stream)
    (write-char #\Space stream)
    (prin1 (slot-value-default user 'name #\?) stream)))

(defmethod serialize nconc ((user user))
  (list :name (name-of user)
        :realname (realname-of user)
        :password (password-of user)))

(defmethod linkable-href ((user user))
  (values
   (call-next-method)
   (name-of user)))

(defmethod storage-update :after ((user user))
  (red:set (lookup-key 'user :name (name-of user))
           (storage-id user)))

(defmethod issues-of ((user user) &key (start 0) (stop -1))
  (storage-read-backrefs 'issue user :start start :stop stop))

(defmethod notes-of ((user user) &key (start 0) (stop -1))
  (storage-read-backrefs 'note user :start start :stop stop))

(defmethod files-of ((user user) &key (start 0) (stop -1))
  (lrange (mapcan #'files-of (notes-of user)) start stop))

(defun correct-password-p (user cleartext)
  (when (and user (stringp cleartext) (< 0 (length cleartext)))
    (when-let (digest (password-of user))
      (ironclad:pbkdf2-check-password
       (ironclad:ascii-string-to-byte-array cleartext)
       digest))))

(defun set-password (user cleartext)
  (let ((digest (ironclad:pbkdf2-hash-password-to-combined-string
                 (ironclad:ascii-string-to-byte-array cleartext))))
    (setf (password-of user) digest)))

(defun get-user-id ()
  (when-let (user-id (hunchentoot:session-value 'user-id))
    (parse-integer user-id)))

(defun get-user ()
  (when-let (user-id (get-user-id))
    (storage-read 'user user-id)))

(defun set-user (user)
  (setf (hunchentoot:session-value 'user-id)
        (when user (storage-id user))))
