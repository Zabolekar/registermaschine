(defpackage #:reg3 (:use #:cl)
  (:export #:*max-steps*
           #:register-machine-lambda
           #:define-register-machine))

(in-package #:reg3)

(defparameter *max-steps* 100) ; set to nil if you want to allow infinite loops

#|

Общая задача: во время компиляции преобразовать код машины в тело функции.

Рассматривается класс машин с синтаксисом ((state command)*), где state ― символ, command ― символ или список. Предполагается, что имеется одно выделенное состояние ― точка входа (по умолчанию ― :start) и одна выделенная команда ― точка выхода (по умолчанию ― :halt). Если команда является символом, отличным от точки выхода, она означает переход к соответствующему состоянию.

В общем и целом, нам хотелось бы генерировать что-то навроде

(lambda (memory)
  (values memory
          (loop for steps from 0
                for state = :start then (cond ((eql :halt command) nil)
                                              ((symbolp command)
                                               command)
                                              (t (ecase (first command)
                                                   (:jeqz (destructuring-bind (n then else) (rest command)
                                                            (if (zerop
                                                                  (aref memory n))
                                                                then
                                                                else)))
                                                   (:dec (destructuring-bind (n next-state) (rest command)
                                                           (progn
                                                             (when
                                                               (plusp
                                                                 (aref memory n))
                                                               (decf (aref memory n)))
                                                             next-state)))
                                                   (:inc (destructuring-bind (n next-state) (rest command)
                                                           (progn
                                                             (incf (aref memory n))
                                                             next-state))))))
                while state
                for command = (second (assoc state
                                             '((a (:jeqz 1 e b))
                                               (b (:jeqz 0 b c))
                                               (c (:dec 0 d))
                                               (d (:dec 1 a))
                                               (e (:jeqz 0 h f))
                                               (f (:dec 0 g))
                                               (g (:inc 2 e))
                                               (h :halt)
                                               (:start a))))
                when (and *max-steps* (> steps *max-steps*)) do (return nil)
                finally (return t))))

причём чтобы набор ветвей в ecase генерировался бы в зависимости от типа машины так, чтобы команды можно было определять отдельно, свои для машины любого типа.

Валидация: проверка, что узлы машины представляют собой список списков нужного вида, что операции имеют смысл для данной машины, и что операции имеют нужное число аргументов нужных типов.

С определениями операций связаны три генеричные функции: 1) возвращающая соответствующий кусок кода; 2) регистрирующая операцию как имеющую смысл для машин данного класса; 3) сигнатура (список типов аргументов). Все три метода генерируются из одного описания операции макросом define-operation.

Конвенции: суффикс -f означает «форма», -v ― «переменная».

Для простоты код в одном файле, поэтому использующиеся в макросах классы и функции завёрнуты в eval-when.

|#

(eval-when (:compile-toplevel :load-toplevel :execute)

  ;;; МАШИНЫ НАИБОЛЕЕ ОБЩЕГО ВИДА

  (defclass machine ()
    ((nodes :accessor nodes :initarg :nodes :documentation "Complete code of the machine.")
     (entry-point :accessor entry-point :initarg entry-point :initform :start :documentation "Initial state.")
     (exit-point :accessor exit-point :initarg entry-point :initform :halt :documentation "Halt command."))
    (:documentation "Instances of of this class incorporate information about the code of particular machines."))

  (defgeneric next-state-case-f (commandv machine)
    (:documentation "Assuming that COMMANDV is the name of a variable containing a command, return correspondent tree of choices defining the next state."))

  (defgeneric run-machine-f (machine)
    (:documentation "Return the Lisp form corresponding to the functioning of MACHINE."))

  (defmethod run-machine-f ((machine machine))
    "For a general machine, we assume that the execution of a command returns the next state according to NEXT-STATE-CASE-F.  If *MAX-STEPS* is a number, the function returns NIL after *MAX-STEPS* steps; otherwise it returns T."
    (let ((steps (gensym "STEPS-"))
          (commandv (gensym "COMMAND-"))
          (state (gensym "STATE-")))
      `(loop for ,steps from 0
             for ,state = ,(entry-point machine) then ,(next-state-case-f commandv machine)
             while ,state
             for ,commandv = (second (assoc ,state ',(nodes machine)))
             when (and *max-steps* (> ,steps *max-steps*)) do (return nil)
             finally (return t))))

  (defgeneric operations (machine)
    (:documentation "The list of all operations used in MACHINE."))

  (defmethod operations ((machine machine))
    (remove-duplicates (loop for node in (nodes machine)
                             when (listp (second node))
                             collect (first (second node)))))

  (defgeneric states (machine)
    (:documentation "The list of all states present in MACHINE."))


  (defmethod states ((machine machine))
    (mapcar #'first (nodes machine)))

  (defgeneric execute-command-f (machine operation commandv)
    (:documentation "The form corresponding to the execution of a command variable COMMANDV with the operation OPERATION.  The form should return the next state or NIL and may have side effects."))

  (defmethod next-state-case-f (commandv (machine machine))
    `(cond ((eql ,(exit-point machine) ,commandv) nil)
           ((symbolp ,commandv) ,commandv)
           (t (ecase (first ,commandv)
                ,@(loop for op in (operations machine)
                        collect `(,op ,(execute-command-f machine op commandv)))))))

  (defgeneric signature (operation machine))

  (defgeneric admissible-operation-p (operation machine))

  (defmethod admissible-operation-p (operation machine)
    nil)

  #|

  Пример того, что мы хотим в качестве execute-command-f:

  (defmethod execute-command-f (((machine register-machine) operation (eql :inc)) commandv)
    `(destructuring-bind (n next-state) (rest ,commandv)
       (incf ,(register-accessor-f n machine))
       next-state))

  |#

  (defmacro define-operation ((operation (machine class)) arguments body)
    (let ((operationv (gensym "OPERATION-"))
          (commandv (gensym "COMMAND-")))
      `(progn
         (defmethod execute-command-f ((,machine ,class) (,operationv (eql ,operation)) ,commandv)
           `(destructuring-bind ,(loop for a in ',arguments
                                       when (listp a) collect (first a)
                                       else collect a) (rest ,,commandv)
              ,,body))
         (defmethod admissible-operation-p ((,operationv (eql ,operation)) (,machine ,class))
           t)
         (defmethod signature ((,operationv (eql ,operation)) (,machine ,class))
           ',(loop for a in arguments
                   when (listp a) collect (second a)
                   else collect t)))))

  (defgeneric machine-body-f (machine)
    (:documentation "The body of a lambda form obtained from the code of MACHINE."))

  ;;; Валидация

(defgeneric check-argument (argument type machine))

(defmethod check-argument (argument (type (eql t)) machine)
  t)

(defmethod check-argument (argument (type (eql :state)) (machine machine))
  (cond ((not (symbolp argument)) (error "~A is not a valid state." argument))
        ((not (member argument (states machine))) (error "State ~A does not exist." argument))))

(defgeneric check-arguments (arguments signature machine))

(defmethod check-arguments (arguments signature (machine machine))
  (unless (= (length arguments) (length signature))
    (error "~D arguments expected, got ~D" (length signature) (length arguments)))
  (loop for argument in arguments
        for type in signature
        do (check-argument argument type machine)))


(defgeneric check-command (command machine))

(defmethod check-command (command (machine machine))
  (cond ((symbolp command)
         (unless (member command (cons (exit-point machine) (states machine)))
           (error "Undefined state ~A" command)))
        ((not (listp command)) (error "Invalid command ~A" command))
        (t (destructuring-bind (operation . arguments) command
             (unless (admissible-operation-p operation machine)
               (error "Command ~A: undefined operation" command))
             (handler-case
               (check-arguments arguments (signature operation machine) machine)
               (simple-error (c) (error "Command ~A: ~A" command c)))))))

(defgeneric check-nodes (machine))

(defmethod check-nodes ((machine machine))
  (let ((nodes (nodes machine)))
    (unless (listp nodes)
      (error "Invalid machine code ~A" nodes))
    (let ((invalid-node (find-if (complement #'listp) nodes)))
      (unless (null invalid-node)
        (error "Invalid node ~A" invalid-node)))
    (let ((malformed-node (find-if #'cddr nodes)))
      (unless (null malformed-node)
        (error "Malformed node ~A" malformed-node)))
    (loop for (state command) in nodes
          for rest-nodes on (rest nodes)
          unless (symbolp state) do (error "Invalid state ~A" state)
          when (member state rest-nodes :key #'first) do (error "Duplicate state ~A" state)
          do (check-command command machine)))
  machine)

  ;;; РЕГИСТРОВЫЕ МАШИНЫ

  (defclass register-machine (machine)
    ((memoryv :accessor memoryv :initarg :memoryv :initform 'memory))
    (:documentation "The instances of the class represent the code of register machines.  The code is to be transformed into the function body depending on the single argument (MEMORYV MACHINE)."))

  (defgeneric register-accessor-f (n machine)
    (:documentation "The form providing the accessor to machine's memory."))

  (defmethod register-accessor-f (n (machine register-machine))
    `(aref ,(memoryv machine) ,n))  


  (define-operation (:inc (machine register-machine)) ((n :register) (next-state :state))
                    `(progn
                       (incf ,(register-accessor-f 'n machine))
                       next-state))

  (define-operation (:dec (machine register-machine)) ((n :register) (next-state :state))
                    `(progn
                       (when (plusp ,(register-accessor-f 'n machine))
                         (decf ,(register-accessor-f 'n machine)))
                       next-state))

  (define-operation (:jeqz (machine register-machine)) ((n :register) (then :state) (else :state))
                    `(if (zerop ,(register-accessor-f 'n machine))
                         then
                         else))
  
(defmethod check-argument (argument (type (eql :register)) (machine register-machine))
  (unless (typep argument '(integer 0))
    (error "~A is not a valid register." argument)))

  ;; TODO add checktype?
  (defmethod machine-body-f ((machine register-machine))
    `(values ,(memoryv machine)
             ,(run-machine-f machine))))

;; TODO add validation
(defmacro register-machine-lambda (&body body)
  "Transforms a machine into a lambda."
  (let ((machine (check-nodes (make-instance 'register-machine :nodes body))))
    `(lambda (,(memoryv machine))
      ,(machine-body-f machine)))) 


(defmacro define-register-machine (name &body body)
  "Transforms a machine into a defun."
  (let ((machine (check-nodes (make-instance 'register-machine :nodes body))))
    `(defun ,name (,(memoryv machine))
      ,(machine-body-f machine)))) 




#|

(define-register-machine adder
  (a (:jeqz 0 d b))
  (b (:dec 0 c))
  (c (:inc 2 a))
  (d (:jeqz 1 g e))
  (e (:dec 1 f))
  (f (:inc 2 d))
  (g :halt)
  (:start a))

(adder (coerce '(1 5 7) 'vector))
#(0 0 13)
T

(define-register-machine subtractor
  (a (:jeqz 1 e b))
  (b (:jeqz 0 b c))
  (c (:dec 0 d))
  (d (:dec 1 a))
  (e (:jeqz 0 h f))
  (f (:dec 0 g))
  (g (:inc 2 e))
  (h :halt)
  (:start a))

(subtractor (coerce '(8 3 0) 'vector))
#(0 0 5)
T

(subtractor (coerce '(3 8 0) 'vector))
#(0 5 0)
NIL

|#

(defclass register-machine-with-tracing (register-machine)
  ())

(defmethod execute-command-f ((machine register-machine-with-tracing) operation commandv)
  `(progn
     (print ,commandv)
     ,(call-next-method)))

(defmacro define-register-machine-with-tracing (name &body body)
  "Transforms a machine into a defun."
  (let ((machine (check-nodes (make-instance 'register-machine-with-tracing :nodes body))))
    `(defun ,name (,(memoryv machine))
      ,(machine-body-f machine))))

#|
(admissible-operation-p :inc (make-instance 'register-machine-with-tracing :nodes '((a (:jeqz 1 e b))
                                                       (b (:jeqz 0 b c))
                                                       (c (:dec 0 d))
                                                       (d (:dec 1 a))
                                                       (e (:jeqz 0 h f))
                                                       (f (:dec 0 g))
                                                       (g (:inc 2 e))
                                                       (h :halt)
                                                       (:start a))
               
               ))
(signature :jeqz)
(check-nodes (make-instance 'register-machine-with-tracing :nodes '((a (:jeqz 1 e b))
                                                       (b (:jeqz 0 b c))
                                                       (c (:dec 0 d))
                                                       (d (:dec 1 a))
                                                       (e (:dec 1 a))
                                                       (e (:jeqz 0 h f))
                                                       (f (:dec 0 g))
                                                       (g (:inc 2 e))
                                                       (h :halt)
                                                       (:start a))
               
               ))
)))
(signature :inc (make-instance 'register-machine-with-tracing :nodes '((a (:jeqz 1 e b))
                                                       (b (:jeqz 0 b c))
                                                       (c (:dec 0 d))
                                                       (d (:dec 1 a))
                                                       (e (:jeqz 0 h f))
                                                       (f (:dec 0 g))
                                                       (g (:inc 2 e))
                                                       (h :halt)
                                                       (:start a))
               
               ))
(execute-command-f(make-instance 'register-machine-with-tracing :nodes '((a (:jeqz 1 e b))
                                                       (b (:jeqz 0 b c))
                                                       (c (:dec 0 d))
                                                       (d (:dec 1 a))
                                                       (e (:jeqz 0 h f))
                                                       (f (:dec 0 g))
                                                       (g (:inc 2 e))
                                                       (h :halt)
                                                       (:start a))
               )  :inc 'command )

(define-register-machine-with-tracing subtractor-with-tracing
  (a (:jeqz 1 e b))
  (b (:jeqz 0 b c))
  (c (:dec 0 d))
  (d (:dec 1 a))
  (e (:jeqz 0 h f))
  (f (:dec 0 g))
  (g (:inc 2 e))
  (h :halt)
  (:start a))

(subtractor-with-tracing (coerce '(8 3 0) 'vector))
|#
