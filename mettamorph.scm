;run with CHICKEN Scheme!
(import srfi-1 srfi-69)   ;filter and hashtable support in Scheme
(import amb amb-extras)   ;amb to implement superpose nesting behavior, amb1 to implement superpose
(import matchable)        ;let/case constructs as well as match-lambda with list deconstruction
(import (chicken flonum)) ;floating point options

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
    ((_ ( (superpose x) ...))
     (amb x ...))
    ((_ arg)
     (auto-list1 arg))))

;; FUNCTION DEFINITION IN METTA

(define functions (make-hash-table))

(define-syntax =
  (syntax-rules ()
    ((_ (name patterni ...) body)
     (begin
       (let ((name (match-lambda* ((patterni ...) (auto-list1 body)))))
            (if (hash-table-exists? functions 'name)
                (hash-table-set! functions 'name (cons name (hash-table-ref functions 'name)))
                (hash-table-set! functions 'name (list name))))
       (set! name (lambda args (handle-exceptions exn ((amb-failure-continuation))
                          (apply (amb1 (hash-table-ref functions 'name)) args))))))))

;; SYNTACTIC CONSTRUCTS FOR DEFINING VARIABLES

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

;; SYNTACTIC CONSTRUCTS FOR CHECKING VARIABLES

(define-syntax Case
  (syntax-rules (else)
    ((_ var ((pati bodi) ...))
     (handle-exceptions exn ((amb-failure-continuation))
                        (match (auto-list1 var) (pati (auto-list1 bodi)) ...)))))

(define-syntax If
  (syntax-rules ()
    ((_ condition thenbody elsebody)
        (if condition (auto-list1 thenbody) (auto-list1 elsebody)))
    ((_ condition thenbody)
        (if condition (auto-list1 thenbody) ((amb-failure-continuation))))))

;; QUERY STATEMENT EXECUTION OPERATOR

(define (print-all xs)
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
     (print-all (amb-collect (auto-list1 args) ...)))))

; AUTO-LIST TO ELIMINATE THE NEED FOR LIST-FUNCTIONCALL DISTINCTION

(define-syntax auto-list-helper
  (syntax-rules ()
    ((_ expr1 ()) ;empty list
     (list expr1))
    ((_ (expr1i ...) argi ...) ;a nested expression is not a procedure
     (cons (auto-list1 (expr1i ...)) (list (auto-list1 argi) ...)))
    ((_ expr1 argi ...)
     (if (procedure? expr1)
         (apply expr1 (list (auto-list1 argi) ...))
         (cons (auto-list1 expr1) (list (auto-list1 argi) ...))))))

(define-syntax is-metta-macro?
  (syntax-rules ()
    ((_ expr1)
     (or (eq? 'expr1 'sequential) (eq? 'expr1 'superpose) (eq? 'expr1 'collapse)
         (eq? 'expr1 'Let) (eq? 'expr1 'Let*) (eq? 'expr1 'Match)
         (eq? 'expr1 'Case) (eq? 'expr1 'If) (eq? 'expr1 '==)
         (eq? 'expr1 'add-atom) (eq? 'expr1 'quote)))))

(define-syntax auto-list
  (syntax-rules ()
    ((_ expr)
     (list (auto-list1 expr)))
    ((_ expr1 expri ...)
     (if (is-metta-macro? expr1)
         (expr1 expri ...)
         (auto-list-helper expr1 expri ...)))))

(define-syntax auto-list1
   (syntax-rules ()
    ((_ (vari ...))
     (auto-list vari ...))
    ((_ var1)
     var1)))

;; EQUALITY

(define == equal?)

;; SPACES IMPLEMENTATION

(define &self '())

(define-syntax add-atom
  (syntax-rules ()
    ((_ space (atomi ...))
     (set! space (cons (list atomi ...) space)))))

(define-syntax Match
  (syntax-rules ()
    ((_ space binds result)
     (match-let* ((binds (amb1 space))) (auto-list1 result)))))

;; PROCEDURAL CONSTRUCTS

(define-syntax sequential ;sequential cannot be superpose in Scheme as in MeTTa
  (syntax-rules ()        ;as procedural sequential execution demands :begin"
    ((_ (expr ...))       ;that's why this construct is defined here instead
     (begin
       expr ...))))
