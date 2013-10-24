(defpackage #:supportcentre-test
  (:use #:cl #:supportcentre #:hu.dwim.stefil)
  (:export #:test-all))

(in-package #:supportcentre-test)

(defsuite* test-all)

(deftest lrange-positive-positive-test ()
  (let ((list '(0 1 2)))
    (flet ((test (start stop expected)
             (is (equal (supportcentre::lrange list start stop) expected))))
      (test 0 2 '(0 1 2))
      (test 1 2 '(1 2))
      (test 2 2 '(2))
      (test 0 1 '(0 1))
      (test 1 1 '(1))
      (test 0 0 '(0)))))

(deftest lrange-positive-negative-test ()
  (let ((list '(0 1 2)))
    (flet ((test (start stop expected)
             (is (equal (supportcentre::lrange list start stop) expected))))
      (test 0 -1 '(0 1 2))
      (test 1 -1 '(1 2))
      (test 2 -1 '(2))
      (test 0 -2 '(0 1))
      (test 1 -2 '(1))
      (test 0 -3 '(0)))))

(deftest lrange-negative-postitive-test ()
  (let ((list '(0 1 2)))
    (flet ((test (start stop expected)
             (is (equal (supportcentre::lrange list start stop) expected))))
      (test -3 2 '(0 1 2))
      (test -2 2 '(1 2))
      (test -1 2 '(2))
      (test -3 1 '(0 1))
      (test -2 1 '(1))
      (test -3 0 '(0)))))

(deftest lrange-negative-negative-test ()
  (let ((list '(0 1 2)))
    (flet ((test (start stop expected)
             (is (equal (supportcentre::lrange list start stop) expected))))
      (test -3 -1 '(0 1 2))
      (test -2 -1 '(1 2))
      (test -1 -1 '(2))
      (test -3 -2 '(0 1))
      (test -2 -2 '(1))
      (test -3 -3 '(0)))))

(deftest lrange-out-of-range-test ()
  (let ((list '(0 1 2)))
    (flet ((test (start stop expected)
             (is (equal (supportcentre::lrange list start stop) expected))))
      (test 0 3 '(0 1 2))
      (test 3 3 nil)
      (test -4 3 '(0 1 2))
      (test -4 -4 nil)
      (test 2 0 nil)
      (test 2 -3 nil)
      (test -1 0 nil)
      (test -1 -3 nil))))