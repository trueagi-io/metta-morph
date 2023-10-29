;; Run with CHICKEN Scheme!
(import srfi-1 srfi-69)    ;filter, hashtable
(import amb amb-extras)    ;amb to implement superpose and amb1 to implements its nesting behavior
(import matchable)         ;let/case constructs as match-lambda with deconstruction
(import (chicken flonum))  ;floating point options
(import (chicken type))    ;type system


;; Type Definitions
(define-type Atom *)
(define-type Symbol symbol)
(define-type Expression list)
(define-type Bool boolean)
(define-type Number number)
(define-type String string)

(define-syntax Typedef
  (syntax-rules ()
    ((_ arg (-> A ... B))
     (cond-expand
       (USE_TYPES (: arg (A ... -> B))) (else '())))
    ((_ arg1 arg2)
     (cond-expand
       (USE_TYPES (define-type arg1 arg2)) (else '())))))


;; Prining the elements / query statement / execution operator
(define-syntax !
  (syntax-rules ()
    ((_ argi ...)
     (print-helper (amb-collect (auto-list argi ...))))))

(define (print-helper xs)
  (display "[")
  (define (print-items xs)
    (cond
      ((null? xs)
       (display "]")
       (newline))
      (else
       (display (car xs))
       (if (not (null? (cdr xs)))
           (display ", "))
       (print-items (cdr xs)))))
  (print-items xs))


;; MeTTa's Let and Let*: Using Scheme's match-let*
(define-syntax Let
  (syntax-rules ()
    ((_ var val body)
     (match-let* ((var (auto-list1 val))) (auto-list1 body)))))

(define-syntax Let*
  (syntax-rules ()
    ((_ ((vari vali) ...) body)
     (match-let* ((vari (auto-list1 vali)) ...) (auto-list1 body)))
    ((_ (((vari1 vari2) vali) ...) body)
     (match-let* (((vari1 vari2) (auto-list1 vali)) ...) (auto-list1 body)))))


;; MeTTa's case expression 
(define-syntax Case
  (syntax-rules (else)
    ((_ var ((pati bodi) ...))
     (handle-exceptions exn ((amb-failure-continuation))
                        (match (auto-list1 var) (pati (auto-list1 bodi)) ...)))))


;; Equality: to allow using '==' for MeTTa compatibility
(define == equal?)


;; Trace 
(define (trace! x y) (begin (display x) y))


;; Collapse: using Scheme's 'amb-collect' implements MeTTa's collapse
(define-syntax collapse
  (syntax-rules ()
    ((_ args)
     (filter (lambda (x) (not (== x (if #f 1)))) ; (if #f 1) can be any number here
             (amb-collect (handle-exceptions exn ((amb-failure-continuation)) args))))))


;; Superpose: using Scheme's ambivalence operator 'amb1' implements MeTTa's superpose.
;;            superpose-helper is for MeTTa's nested superpose statements
(define-syntax superpose
  (syntax-rules ()
    ((_ args)
     (amb1 (superpose-helper args)))))

(define-syntax superpose-helper
  (syntax-rules ()
    ((_ ((superpose x) ...))
     (amb x ...))
    ((_ arg)
     (auto-list1 arg))))


;; Hash-Tables: for keeping Functions, States and Spaces 
(define functions (make-hash-table))
(define vars (make-hash-table))
(hash-table-set! vars '&self '())


;; MeTTa function definition: Creates a hash map where the MeTTa functions with all 
;; arguments are registered as keys. The values are the lists containing function's
;; bodies to allow functions with the same definition to appear more than once
(define-syntax =
  (syntax-rules ()
    ((_ (name patterni ...) body)
     (begin
       (hash-table-set! vars '&self (cons '(=def (name patterni ...) body) (hash-table-ref vars '&self)))
       (let ((name (match-lambda* ((patterni ...) (auto-list1 body)))))
            (if (hash-table-exists? functions 'name)
                (hash-table-set! functions 'name (cons name (hash-table-ref functions 'name)))
                (hash-table-set! functions 'name (list name))))
       (set! name (lambda args (handle-exceptions exn ((amb-failure-continuation))
                          (apply (amb1 (hash-table-ref functions 'name)) args))))))))


;; MeTTa match expression: using Scheme's match-let* 
(define-syntax Match
  (syntax-rules (MatchChain)
    ((_ space (MatchChain bind1 bind2) result)
     (Match space bind1 (Match space bind2 result)))
    ((_ space (MatchChain bind1 bindi ...) result)
     (Match space bind1 (Match space (MatchChain bindi ...) result)))
    ((_ space binds result)
     (handle-exceptions exn ((amb-failure-continuation))
       (match-let* ((binds (amb1 (hash-table-ref vars space)))) (auto-list1 result))))))


;; Spaces & States implementation: addition, removal, binding; changing and getting state
(define (new-space S) S)
(define (new-state S) S)
(define (get-state S) S)
(define (get-atoms S) (amb1 (hash-table-ref vars S)))

(define (add-atom space atom)
  (begin (hash-table-set! vars space (cons atom (hash-table-ref vars space))) '()))

(define (remove-atom space atom)
  (begin (hash-table-set! vars space (delete atom (hash-table-ref vars space))) '()))

(define (bind! var val)
  (begin (hash-table-set! vars var val) '()))

(define (change-state! var val)
  (begin (hash-table-set! vars var val) (list 'State val)))

(define (get-state var)
  (hash-table-ref vars var))


;; Found in NARS Utils as alternative 'if' statement 
(define-syntax If
  (syntax-rules ()
    ((_ condition thenbody elsebody)
        (if condition (auto-list1 thenbody) (auto-list1 elsebody)))
    ((_ condition thenbody)
        (if condition (auto-list1 thenbody) ((amb-failure-continuation))))))


;; Found in NARS Utils for procudural (sequential) constructs. In Scheme sequential
;; cannot be "superposed" as in MeTTa. Procedural sequential execution uses "begin"
(define-syntax sequential-helper
  (syntax-rules (do)
    ((_ (do expr))
     expr)
    ((_ expr)
     (set! ret (append ret (list expr))))))

(define-syntax sequential ;sequential cannot be superpose in Scheme as in MeTTa
  (syntax-rules ()        ;as procedural sequential execution demands "begin"
    ((_ (expri ...))      ;that's why this construct is defined here instead
     (begin
       (set! ret '())
       (sequential-helper (auto-list1 expri)) ...
       (amb1 ret)))))


;; AUTO-LIST to eliminate the need for distinguishing LISTs from FUNCTION call
;; Returns a list if arg is a list or executes a function if arg is a function
;; To avoid expansion of a full algorithmic tree and make compiling efficient
;; MeTTa macros definition is used to limit expansion only to the defined macros
(define-syntax metta-macro-if
  (syntax-rules (collapse superpose Let Let* Match Case If == sequential quote)
    ((_ collapse then else) then)
    ((_ superpose then else) then)
    ((_ Let then else) then)
    ((_ Let* then else) then)
    ((_ Match then else) then)
    ((_ Case then else) then)
    ((_ If then else) then)
    ((_ == then else) then)
    ((_ sequential then else) then)
    ((_ quote then else) then)
    ((_ arg then else) else)))

(define-syntax auto-list-helper
  (syntax-rules ()
    ((_ expr1 ()) ;empty list
     (list expr1))
    ((_ (expr1i ...) argi ...) ;a nested expression is not a procedure
     (list (auto-list expr1i ...) (auto-list1 argi) ...))
    ((_ expr1 argi ...)
     (if (procedure? expr1)
         (apply expr1 (list (auto-list1 argi) ...))
         (list (auto-list1 expr1) (auto-list1 argi) ...)))))

(define-syntax auto-list
  (syntax-rules ()
    ((_ expr)
     (list (auto-list1 expr)))
    ((_ expr1 expri ...)
     (metta-macro-if expr1
         (expr1 expri ...)
         (auto-list-helper expr1 expri ...)))))

(define-syntax auto-list1
   (syntax-rules ()
    ((_ (vari ...))
     (auto-list vari ...))
    ((_ var1)
     var1)))

