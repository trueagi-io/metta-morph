;; Run with CHICKEN Scheme!
(import srfi-1 srfi-13 srfi-69)    ;filter, hashtable
(import amb amb-extras)    ;amb to implement superpose and amb1 to implements its nesting behavior
(import matchable)         ;let/case constructs as match-lambda with deconstruction
(import (chicken flonum))  ;floating point options
(import (chicken type))    ;type system
(import (chicken string))  ;string conversion

;; COLLAPSE AND SUPERPOSE

(define-syntax collapse
  (syntax-rules ()
    ((_ args)
     (filter (lambda (x) (not (== x (if #f 42))))
             (amb-collect (handle-exceptions exn ((amb-failure-continuation)) args))))))

(define-syntax superpose
  (syntax-rules ()
    ((_ args)
     (amb1 (superpose-helper args)))))

(define-syntax superpose-helper
  (syntax-rules ()
    ((_ ((superpose x) ...))
     (amb x ...))
    ((_ arg)
     (auto-quote-list1-with-eval #t arg))))

;; FUNCTION DEFINITION IN METTA

(define functions (make-hash-table))

(define-syntax testmacro
  (syntax-rules ()
    ((_ vari ...)
     (vari ...))))

(define-syntax =helper
  (syntax-rules ()
    ((_ (name patterni ...) body)
     (begin
       (set! &self (cons '(=def (name patterni ...) body) &self)) ;for = in match
       (let ((name (match-lambda* ((patterni ...) (auto-quote-list1-with-eval #t body)))))
            (if (hash-table-exists? functions 'name)
                (hash-table-set! functions 'name (cons name (hash-table-ref functions 'name)))
                (hash-table-set! functions 'name (list name))))
       (set! name (lambda args (handle-exceptions exn ((amb-failure-continuation))
                          (apply (amb1 (hash-table-ref functions 'name)) args))))))))

(define-syntax =
  (syntax-rules (ok)
    ((_ (ok name patterni ...) body)
     (=helper (name patterni ...) body))
    ((_ (name patterni ...) body)
     (= (ok name (testmacro patterni) ...) body))))

;; SYNTACTIC CONSTRUCTS FOR DEFINING VARIABLES

(define-syntax Let
  (syntax-rules ()
    ((_ var val body)
     (match-let* ((var (auto-quote-list1-with-eval #t val))) (auto-quote-list1-with-eval #t body)))))

(define-syntax Let*
  (syntax-rules ()
    ((_ ((vari vali) ...) body)
     (match-let* ((vari (auto-quote-list1-with-eval #t vali)) ...) (auto-quote-list1-with-eval #t body)))
    ((_ (((vari1 vari2) vali) ...) body)
     (match-let* (((vari1 vari2) (auto-quote-list1-with-eval #t vali)) ...) (auto-quote-list1-with-eval #t body)))))

;; SYNTACTIC CONSTRUCTS FOR CHECKING VARIABLES

(define-syntax Case
  (syntax-rules (else)
    ((_ var ((pati bodi) ...))
     (handle-exceptions exn ((amb-failure-continuation))
                        (match (auto-quote-list1-with-eval #t var) (pati (auto-quote-list1-with-eval #t bodi)) ...)))))

(define-syntax If
  (syntax-rules ()
    ((_ condition thenbody elsebody)
        (if condition (auto-quote-list1-with-eval #t thenbody) (auto-quote-list1-with-eval #t elsebody)))
    ((_ condition thenbody)
        (if condition (auto-quote-list1-with-eval #t thenbody) ((amb-failure-continuation))))))

;; QUERY STATEMENT EXECUTION OPERATOR

(define (print-solutions xs)
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

(define-syntax !
  (syntax-rules ()
    ((_ args ...)
     (print-solutions (amb-collect (auto-quote-list1-with-eval #t (args ...)))))))

;; AUTO-LIST TO ELIMINATE THE NEED FOR LIST-FUNCTIONCALL DISTINCTION

(define-syntax metta-macro-if
  (syntax-rules (collapse superpose Let Let* Match Case If == add-atom remove-atom bind! change-state! sequential quote)
    ((_ collapse then else) then)
    ((_ superpose then else) then)
    ((_ Let then else) then)
    ((_ Let* then else) then)
    ((_ Match then else) then)
    ((_ Case then else) then)
    ((_ If then else) then)
    ((_ == then else) then)
    ((_ add-atom then else) then)
    ((_ remove-atom then else) then)
    ((_ bind! then else) then)
    ((_ change-state! then else) then)
    ((_ sequential then else) then)
    ((_ quote then else) then)
    ((_ arg then else) else)))

(define-syntax auto-quote
 (syntax-rules ()
    ((_ (argi ...))
     (argi ...))
    ((_ arg)
     (handle-exceptions exn (quote arg)
        (if (or (number? arg) (string? arg) (boolean? arg) (string-prefix? "$" (symbol->string 'arg)))
            arg
            (quote arg))))))

(define-syntax auto-list-with-eval-helper
  (syntax-rules ()
    ((_ EVAL expr1 ()) ;empty list
     (list expr1))
    ((_ EVAL (expr1i ...) argi ...) ;a nested expression is not a procedure
     (list (auto-quote-list1-with-eval EVAL (expr1i ...)) (auto-quote-list1-with-eval EVAL argi) ...))
    ((_ EVAL expr1 argi ...)
     (handle-exceptions exn 
       (if EVAL 
           (list (auto-quote-list1-with-eval EVAL expr1) (auto-quote-list1-with-eval EVAL argi) ...)
           ((auto-quote-list1-with-eval EVAL expr1) (auto-quote-list1-with-eval EVAL argi) ...)
           )
       (if EVAL
           (if (procedure? expr1)
               (apply expr1 (list (auto-quote-list1-with-eval EVAL argi) ...))
               (list (auto-quote-list1-with-eval EVAL expr1) (auto-quote-list1-with-eval EVAL argi) ...))
           ((auto-quote-list1-with-eval EVAL expr1) (auto-quote-list1-with-eval EVAL argi) ...))))))

(define-syntax auto-list-with-eval
  (syntax-rules ()
    ((_ EVAL expr)
     (list (auto-list1-with-eval EVAL expr)))
    ((_ EVAL expr1 expri ...)
     (metta-macro-if expr1
       (expr1 expri ...)
       (if EVAL
           (auto-list-with-eval-helper EVAL expr1 expri ...)
           (expr1 expri ...))))))

(define-syntax auto-list1-with-eval
   (syntax-rules ()
    ((_ EVAL (vari ...))
     (auto-list-with-eval EVAL vari ...))
    ((_ EVAL var1)
     var1)))

(define-syntax auto-quote-list1-with-eval
   (syntax-rules ()
    ((_ EVAL (vari ...))
     (auto-quote (auto-list1-with-eval EVAL (vari ...))))
    ((_ EVAL var1)
     (auto-quote var1))))

;; EQUALITY

(define == equal?)

;; TYPE SYSTEM

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

;; SPACES IMPLEMENTATION

(define &self '())
(define (new-space S) S)
(define (new-state S) S)
(define (get-state S) S)
(define (get-atoms S) (amb1 S))

(define-syntax add-atom
  (syntax-rules ()
    ((_ space atom)
     (begin (set! space (cons (auto-quote-list1-with-eval #t atom) space)) '()))))

(define-syntax remove-atom
  (syntax-rules ()
    ((_ space atom)
     (begin (let ((atm (auto-quote-list1-with-eval #t atom))) (set! space (delete atm space)) '())))))

(define-syntax bind!
  (syntax-rules ()
    ((_ space val)
     (begin (set! space (auto-quote-list1-with-eval #t val)) '()))))

(define-syntax change-state!
  (syntax-rules ()
    ((_ var val)
     (begin (set! var (auto-quote-list1-with-eval #t val)) (list 'State val)))))

(define-syntax Match
  (syntax-rules (MatchChain)
    ((_ space (MatchChain bind1 bind2) result)
     (Match space bind1 (Match space bind2 result)))
    ((_ space (MatchChain bind1 bindi ...) result)
     (Match space bind1 (Match space (MatchChain bindi ...) result)))
    ((_ space binds result)
     (handle-exceptions exn ((amb-failure-continuation))
                        (match-let* ((binds (amb1 space))) (auto-quote-list1-with-eval #t result))))))

;; PROCEDURAL CONSTRUCTS

(define-syntax sequential-helper
  (syntax-rules (do)
    ((_ (do expr))
     expr)
    ((_ expr)
     (set! ret (append ret (list expr))))))

(define-syntax sequential ;sequential cannot be superpose in Scheme as in MeTTa
  (syntax-rules ()        ;as procedural sequential execution demands :begin"
    ((_ (expri ...))      ;that's why this construct is defined here instead
     (begin
       (set! ret '())
       (sequential-helper (auto-quote-list1-with-eval #t expri)) ...
       (amb1 ret)))))
