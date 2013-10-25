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
    (dolist (thing (storage-read-set type type :all))
      (format stream "~&~a~a~a"
              prefix
              (if port (format nil ":~d" port) "")
              (href thing)))))

(defun latin1-to-utf8 (string)
  (flex:octets-to-string
   (flex:string-to-octets string :external-format :latin1)
   :external-format :utf-8))

(defun lrange (list start stop)
  "Returns a new list with the specified elements. The offsets start
and stop are zero-based indexes.

These offsets can also be negative numbers indicating offsets starting
at the end of the list. For example, -1 is the last element of the
list, -2 the penultimate, and so on.

Out of range indexes will not produce an error. If start is larger
than stop or the end of the list, an empty list is returned. If stop
is larger than the actual end of the list, it is treated like the last
element of the list."
  (let (len)
    (labels ((len ()
               (or len (setf len (length list))))
             (pos (i)
               (if (minusp i)
                   (+ (len) i)
                   i))
             (take (start stop)
               (loop for x in list
                     for i from 0 upto stop
                     when (<= start i)
                       collect x)))
      (cond ((= -1 stop)
             (nthcdr (pos start) list))
            ((and (<= 0 start) (<= 0 stop))
             (take start stop))
            (t
             (take (pos start) (pos stop)))))))
