(in-package #:supportcentre)

(defun safe-read (string)
  (let ((*read-eval* nil))
    (read-from-string string)))

(defun as-text (object)
  (hunchentoot:escape-for-html
   (with-output-to-string (out)
     (prin1 object out))))

(defun compile-templates ()
  (closure-template:compile-cl-templates
   (directory
    (merge-pathnames "templates/*.tmpl"
                     (asdf:component-pathname (asdf:find-system '#:supportcentre))))))

(defun slot-value-default (object slot-name default)
  (if (slot-boundp object slot-name)
      (slot-value object slot-name)
      default))

(defun print-urls (&key (stream *standard-output*)
                     (prefix "http://localhost")
                     (port nil))
  (dolist (type '(area issue note user file))
    (dolist (thing (storage-read-set type :all))
      (format stream "~&~a~a~a"
              prefix
              (if port (format nil ":~d" port) "")
              (href thing)))))
