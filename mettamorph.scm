;run with CHICKEN Scheme!
(import (chicken condition)) ;exceptional chicken
(import srfi-1) ;filter
(import srfi-13) ;string support in Scheme
(import srfi-69) ;hashtable support in Scheme
(import amb)     ;amb to implement superpose nesting behavior
(import amb-extras) ;amb1 to implement superpose
(import matchable) ;let/case constructs with list deconstruction
(import bindings) ;bind-case with deconstruction

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

(define (notUnspecified x)
        (not (== x (if #f 42))))

(define-syntax collapse
  (syntax-rules ()
    ((_ args)
     (filter notUnspecified (amb-collect (handle-exceptions exn ((amb-failure-continuation)) args))))))

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

(define-syntax =
  (syntax-rules ()
    ((_ (name patterns ...) body)
     (begin
       (let ((name (match-lambda* ((patterns ...) body))))
         (if (hash-table-exists? functions 'name)
             (hash-table-set! functions 'name (cons name (hash-table-ref functions 'name)))
             (hash-table-set! functions 'name (list name))))
       (set! name (lambda args (handle-exceptions exn ((amb-failure-continuation))
                          (apply (amb1 (hash-table-ref functions 'name)) args))))))))

(define-syntax !
  (syntax-rules ()
    ((_ args ...)
     (print-all (amb-collect args ...)))))

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
     (handle-exceptions exn ((amb-failure-continuation)) (bind-case var (pat body) ...)))))

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

(define-syntax If
  (syntax-rules ()
    ((_ condition thenbody elsebody)
        (if condition thenbody elsebody))
    ((_ condition thenbody)
        (if condition thenbody '()))))

(define-syntax MatchMetta
  (syntax-rules ()
    ((_ space binds result)
     (match-let* ((binds (amb1 space))) result))))
