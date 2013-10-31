(in-package #:supportcentre)

(defclass issue (storable linkable)
  ((subject :initarg :subject :accessor subject-of)
   (creator :initarg :creator :accessor user-of)
   (area :initarg :area :accessor area-of)
   (status :initarg :status :initform :open :accessor status-of)
   (priority :initarg :priority :initform :information :accessor priority-of)))

(defparameter *issue-statuses* '(:open :resolved :closed))
(defparameter *issue-priorities* '(:information :low :high :very-high))

(defmethod storage-dependencies ((type (eql 'issue)))
  '((user-of user)
    (area-of area)))

(defmethod serialize nconc ((issue issue))
  (list :subject (subject-of issue)
        :creator (storage-id (user-of issue))
        :area (storage-id (area-of issue))
        :status (status-of issue)
        :priority (priority-of issue)))

(defmethod notes-of ((issue issue) &rest keys)
  (apply #'storage-read-backrefs 'note issue keys))

(defmethod linkable-href ((issue issue))
  (values
   (restas:genurl 'issue
                  :area-id (storage-id (area-of issue))
                  :issue-id (storage-id issue))
   (subject-of issue)))

(defmethod storage-update :after ((issue issue))
  (storage-update (area-of issue))
  (redis:with-pipelining
    (update-sets 'issue issue (status-of issue) *issue-statuses*)
    (update-sets 'issue issue (priority-of issue) *issue-priorities*)))

(defun update-sets (owner thing value possible-values)
  (dolist (non-value (remove value possible-values))
    (storage-set-remove owner non-value thing))
  (storage-set-add owner value thing))

(defmethod supportcentre-markdown:issue-link (id)
  (let ((issue (storage-read 'issue id)))
    (if issue
        (multiple-value-bind (url subject) (linkable-href issue)
            (link url (format nil "Issue #~d: ~a" id subject)))
        (format nil "Issue #~d" id))))
