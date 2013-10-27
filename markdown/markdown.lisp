(in-package #:supportcentre-markdown)

(defrule dec-digit (or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(defrule issue (and #\$ (+ dec-digit))
  (:destructure (dollar digits)
    (declare (ignore dollar))
    (list 'issue (parse-integer (text digits)))))

(define-extension-inline *issue-links* issue-link
    issue
  (:character-rule issue-link-extended-chars #\$)
  (:after 3bmd-grammar:emph))

(setf *issue-links* t)

(defgeneric issue-link (id))

(defmethod print-tagged-element ((tag (eql 'issue)) stream rest)
  (let ((id (first rest)))
    (write-string (issue-link id) stream)))

(defun markdown (source)
  (with-output-to-string (out)
    (3bmd:parse-string-and-print-to-stream source out)))
