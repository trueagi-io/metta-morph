;run with CHICKEN Scheme!
(import srfi-1 srfi-69)  ;filter and hashtable support in Scheme
(import amb amb-extras)  ;amb to implement superpose nesting behavior, amb1 to implement superpose
(import matchable) ;let/case constructs as well as match-lambda with list deconstruction
(import (chicken flonum))   ;floating point options

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
    ((_ (list (superpose x) ...))
     (amb x ...))
    ((_ arg)
     arg)))

(define functions (make-hash-table))
(define-syntax =helper
  (syntax-rules ()
    ((_ (name patterns ...) body)
     (begin
       (let ((name (match-lambda* ((patterns ...) body))))
         (if (hash-table-exists? functions 'name)
             (hash-table-set! functions 'name (cons name (hash-table-ref functions 'name)))
             (hash-table-set! functions 'name (list name))))
       (set! name (lambda args (handle-exceptions exn ((amb-failure-continuation))
                          (apply (amb1 (hash-table-ref functions 'name)) args))))))))

(define-syntax =
  (syntax-rules ()
    ((_ (name patterns ...) (bodi ...))
     (=helper (name patterns ...) (auto-list bodi ...)))
    ((_ (name patterns ...) body)
     (=helper (name patterns ...) body))))

(define-syntax !
  (syntax-rules ()
    ((_ (argi ...) ...)
     (print-all (collapse (auto-list argi ...) ...)))))

(define-syntax LetMetta
  (syntax-rules ()
    ((_ varval body)
     (match-let* varval body))
    ((_ var val body)
     (match-let* ((var val)) body))))

(define-syntax Let*Metta
  (syntax-rules ()
    ((_ ((vari vali) ...) body)
     (match-let* ((vari vali) ...) body)) 
    ((_ (((vari1 vari2) vali) ...) body)
     (match-let* (((vari1 vari2) vali) ...) body))))

(define-syntax CaseMetta
  (syntax-rules (else)
    ((_ var ((pat body) ...))
     (handle-exceptions exn ((amb-failure-continuation)) (match var (pat body) ...)))))

(define &self '())

(define-syntax add-atom
  (syntax-rules ()
    ((_ space (atomi ...))
     (set! space (cons (list atomi ...) space)))))

(define == equal?) ;allow use == for MeTTa compatibility

(define-syntax sequential ;sequential cannot be superpose in Scheme as in MeTTa
  (syntax-rules ()        ;as procedural sequential execution demands :begin"
    ((_ (expr ...))       ;that's why this construct is defined here instead
     (begin
       expr ...))))

(define (auto-list-helper expr1 . args)
  (if (procedure? expr1)
      (apply expr1 args)
      (cons expr1 args)))

(define-syntax is-metta-macro?
  (syntax-rules ()
    ((_ expr1)
     (or (eq? 'expr1 'sequential) (eq? 'expr1 'superpose)
         (eq? 'expr1 'LetMetta) (eq? 'expr1 'Let*Metta)
         (eq? 'expr1 'Let*Metta) (eq? 'expr1 'CaseMetta)
         (eq? 'expr1 'add-atom)))))

(define-syntax auto-list
  (syntax-rules ()
    ((_ exi ...)
     (exi ...))
    ((_ expr1 expr2 expr3)
     (if (or (is-metta-macro? expr1) (eq? 'expr1 '==))
         (expr1 expr2 expr3)
         (auto-list-helper expr1 expr2 expr3)))
    ((_ expr1 expr2 expr3 expr4)
     (if (or (is-metta-macro? expr1) (eq? 'expr1 'If))
         (expr1 expr2 expr3 expr4)
         (auto-list-helper expr1 expr2 expr3 expr4)))
    ((_ expr1 expri ...)
     (if (is-metta-macro? expr1)
         (expr1 expri ...)
         (auto-list-helper expr1 expri ...)))))

(define-syntax If
  (syntax-rules ()
    ((_ condition (theni ...) (elsi ...))
        (if condition (auto-list theni ...) (auto-list elsi ...)))
    ((_ condition (theni ...) else)
        (if condition (auto-list theni ...) else))
    ((_ condition then (elsi ...))
        (if condition then (auto-list elsi ...)))
    ((_ condition (theni ...))
        (if condition (auto-list theni ...) ((amb-failure-continuation))))
    ((_ condition thenbody elsebody)
        (if condition thenbody elsebody))
    ((_ condition thenbody)
        (if condition thenbody))))

(define-syntax MatchMetta
  (syntax-rules ()
    ((_ space binds (resulti ...))
     (match-let* ((binds (amb1 space))) (auto-list-helper resulti ...))) ;why helper required, only functions here?
    ((_ space binds result)
     (match-let* ((binds (amb1 space))) result))))
