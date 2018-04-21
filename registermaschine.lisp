(defpackage #:reg2 (:use #:cl)
  (:export #:machine))

(in-package #:reg2)

(defparameter *max-steps* 100) ; set to nil if you want to allow infinite loops

(defun run-node (machine node memory)
  (cond ((eq :halt node) nil)
        ((symbolp node) (gethash node machine))
        (t (ecase (first node)
             (:inc (let ((n (second node)))
                     (incf (gethash n memory 0))
                     (gethash (third node) machine)))
             (:dec (let ((n (second node)))
                     (when (plusp (gethash n memory 0))
                       (decf (gethash n memory)))
                     (gethash (caddr node) machine)))
             (:jeqz (let ((n (second node)))
                      (gethash (if (zerop (gethash n memory 0))
                                   (third node)
                                   (fourth node))
                               machine)))))))

(defun run-nodes (machine memory)
  (let ((memory-hash (make-hash-table)))
    (loop for i from 0
          for item in memory
          do (setf (gethash i memory-hash) item))
    (loop for steps from 0
          for current = (gethash :start machine) then (run-node machine current memory-hash)
          do
          (when (null current)
            (return (loop for i from 0
                          for item = (gethash i memory-hash)
                          while item
                          collect item)))
          (when (and *max-steps* (> steps *max-steps*))
            (return :bottom)))))

(defmacro machine (&rest ops)
  (let ((machine (gensym "MACHINE-")))
    `(let ((,machine (make-hash-table)))
       ,@(loop for op in ops collect
               `(setf (gethash ',(first op) ,machine) ',(second op)))
       ,machine)))

(defparameter *adder*
  (machine
    (a (:jeqz 0 d b))
    (b (:dec 0 c))
    (c (:inc 2 a))
    (d (:jeqz 1 g e))
    (e (:dec 1 f))
    (f (:inc 2 d))
    (g :halt)
    (:start a)))

(defparameter *subtractor*
  (machine
    (a (:jeqz 1 e b))
    (b (:jeqz 0 b c))
    (c (:dec 0 d))
    (d (:dec 1 a))
    (e (:jeqz 0 h f))
    (f (:dec 0 g))
    (g (:inc 2 e))
    (h :halt)
    (:start a)))

(print (run-nodes *adder* '(1 5 7)))
(print (run-nodes *subtractor* '(8 3)))
(print (run-nodes *subtractor* '(3 8)))
