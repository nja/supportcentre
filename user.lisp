(in-package #:supportcentre)

(defclass user (storable linkable)
  ((name :initarg :name :initform "" :accessor name-of)
   (realname :initarg :realname :initform "" :accessor realname-of)
   (password :initarg :password :initform nil :accessor password-of)))

(defmethod serialize ((user user))
  (with-output-to-string (out)
    (prin1 (list :name (name-of user)
                 :realname (realname-of user)
                 :password (password-of user))
           out)))

(defmethod storage-update :after ((user user))
  (red:set (lookup-key 'user 'name (name-of user))
           (storage-id user)))

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

(defun get-user ()
  (when-let (user-id (hunchentoot:session-value 'user-id))
    (storage-read 'user user-id)))

(defun set-user (user)
  (setf (hunchentoot:session-value 'user-id)
        (and user (storage-id user))))
