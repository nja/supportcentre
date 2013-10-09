(in-package #:supportcentre)

(defun safe-read (string)
  (let ((*read-eval* nil))
    (read-from-string string)))

(defun your-session (&optional stream)
  (let ((session hunchentoot:*session*))
    (if session
        (format stream "Your session is ~D" (hunchentoot:session-id session))
        (format stream "You have no session"))))

(defun as-text (object)
  (hunchentoot:escape-for-html
   (with-output-to-string (out)
     (prin1 object out))))
