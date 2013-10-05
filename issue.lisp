(in-package #:supportcentre)

(defun save-issue (issue)
  (let ((id (red:incr "issue:next-id")))
    (red:sadd "issue:ids" id)
    (red:set (make-key :issue id)
             (serialize-list issue))
    id))

(defun get-all-issues ()
  (let ((ids (get-issue-ids)))
    (mapcar (lambda (issue id)
              (set-id (deserialize-list issue)
                      id))
            (redis:with-pipelining
              (dolist (id ids)
                (red:get (make-key :issue id))))
            ids)))

(defun get-issue-ids ()
  (red:smembers "issue:ids"))

(defun get-issue (id)
  (let ((issue (deserialize-list (red:get (make-key :issue id)))))
    (when issue
      (set-id issue id))))
