;;;; ranges.lisp
;;;; $Id: page-ranges.lisp,v 1.2 2007/09/07 17:34:34 xach Exp $

(defpackage #:page-ranges
  (:export #:map-range
           #:map-range*
           #:page-ranges)
  (:use #:cl))

(in-package #:page-ranges)

(defclass range ()
  ((start :initarg :start :reader start)
   (end :initarg :end :reader end)))

(defmethod print-object ((range range) stream)
  (print-unreadable-object (range stream :type t)
    (format stream "~D:~D" (start range) (end range))))

(defun range (start end)
  (make-instance 'range :start start :end end))

(defun midpoint (range)
  (values (floor (+ (start range) (end range)) 2)))

(defun contract (range amount)
  (let ((midpoint (midpoint range)))
    (range (min midpoint (+ (start range) amount))
           (max midpoint (- (end range) amount)))))

(defun expand (range amount)
  (contract range (- amount)))

(defun clamp (range value)
  (max (min value (end range))
       (start range)))

(defun intersect (a b)
  (range (clamp b (start a))
         (clamp b (end a))))

(defun overlapsp (a b)
  (and (<= (start a) (end b))
       (<= (start b) (end a))))

(defun adjacentp (a b)
  (or  (= (1+ (end b)) (start a))
       (= (1+ (end a)) (start b))))

(defun joinablep (a b)
  (or (adjacentp a b)
      (overlapsp a b)))

(defun join (a b)
  (range (min (start a) (start b))
         (max (end a) (end b))))

(defun join-all (ranges)
  (when ranges
    (setf ranges (sort (copy-list ranges) #'< :key #'midpoint))
    (let ((result (list (pop ranges))))
      (loop
       (when (endp ranges)
         (return (nreverse result)))
       (let ((next (pop ranges)))
         (if (joinablep (first result) next)
             (push (join next (pop result)) result)
             (push next result)))))))

(defun centered-range (midpoint radius)
  (expand (range midpoint midpoint) radius))

(defun page-ranges (selected page-count
                    &key (window-radius 3)
                    (bookend-radius 2))
  (let ((bookstart (centered-range 1 bookend-radius))
        (bookend (centered-range page-count bookend-radius))
        (all-pages (range 1 page-count)))
    (let* ((window-midpoint-range (contract all-pages (+ bookend-radius
                                                         window-radius)))
           (window-midpoint (clamp window-midpoint-range selected))
           (window (expand (range window-midpoint window-midpoint)
                           window-radius)))
      (mapcar (lambda (range)
                (intersect range all-pages))
              (join-all (list bookstart window bookend))))))

(defun map-range (fun range)
  (do ((end (end range))
       (i (start range) (1+ i)))
      ((< end i))
    (funcall fun i)))

(defun map-range* (fun range)
  (do ((end (end range))
       (start (start range))
       (i (start range) (1+ i)))
      ((< end i))
    (funcall fun i (= i start) (= i end))))
