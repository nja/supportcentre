(in-package #:supportcentre)

(defun make-key (prefix suffix)
  (format nil "~a:~a" (symbol-name prefix) suffix))

(defun serialize-list (list)
  (with-output-to-string (out)
    (print list out)))

(defun deserialize-list (string)
  (when string
    (let ((*read-eval* nil))
      (read-from-string string))))

(defun set-id (plist id)
  (nconc (list :id id) plist))
