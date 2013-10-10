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

(defun unique (list &key (test 'eql))
  (loop with seen = (make-hash-table :test test)
        for thing in list
        for present-p = (second (multiple-value-list (gethash thing seen)))
        unless present-p
          collect (setf (gethash thing seen) thing)))
