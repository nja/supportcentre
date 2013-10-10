(in-package #:supportcentre)

(defclass user (storable linkable)
  ((name :initarg :name :initform "" :accessor user-name)
   (realname :initarg :realname :initform "" :accessor user-realname)
   (password :initarg :password :initform nil :accessor user-password)))

(defmethod serialize ((user user))
  (with-output-to-string (out)
    (prin1 (list :name (user-name user)
                 :realname (user-realname user)
                 :password (user-password user))
           out)))

(defmethod storage-update :after ((user user))
  (red:set (lookup-key 'user 'name (user-name user))
           (storage-id user)))

(defun correct-password-p (user cleartext)
  (when (and user (stringp cleartext) (< 0 (length cleartext)))
    (when-let (digest (user-password user))
      (ironclad:pbkdf2-check-password
       (ironclad:ascii-string-to-byte-array cleartext)
       digest))))

(defun set-password (user cleartext)
  (let ((digest (ironclad:pbkdf2-hash-password-to-combined-string
                 (ironclad:ascii-string-to-byte-array cleartext))))
    (setf (user-password user) digest)))
