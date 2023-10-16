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

(define-syntax define-partial
  (syntax-rules ()
    ((_ (name xi ...) body)
     (begin (let ((name (lambda (xi ...) (handle-exceptions exn ((amb-failure-continuation)) body))))
                 (if (hash-table-exists? functions 'name)
                     (hash-table-set! functions 'name (cons name (hash-table-ref functions 'name)))
                     (hash-table-set! functions 'name (list name))))
            (set! name (lambda (xi ...) ((amb1 (hash-table-ref functions 'name)) xi ...)))))))

(define-syntax =
  (syntax-rules () ;(extend if necessary!)
    ((_ (name arg1) body) ;deconstruct 1 argument
     (begin (define-partial (name $T1)
                            (match-let* ((arg1 $T1)) body))))
    ((_ (name arg1 arg2) body) ;deconstruct 2 arguments
     (begin (define-partial (name $T1 $T2)
                            (match-let* (((arg1 arg2) (list $T1 $T2))) body))))
    ((_ (name arg1 arg2 arg3) body) ;deconstruct 3 arguments
     (begin (define-partial (name $T1 $T2 $T3)
                            (match-let* (((arg1 arg2 arg3) (list $T1 $T2 $T3))) body))))
    ((_ (name arg1 arg2 arg3 arg4) body) ;deconstruct 4 arguments
     (begin (define-partial (name $T1 $T2 $T3 $T4)
                            (match-let* (((arg1 arg2 arg3 arg4) (list $T1 $T2 $T3 $T4))) body))))
    ((_ (name arg1 arg2 arg3 arg4 arg5) body) ;deconstruct 5 arguments
     (begin (define-partial (name $T1 $T2 $T3 $T4 $T5)
                            (match-let* (((arg1 arg2 arg3 arg4 arg5) (list $T1 $T2 $T3 $T4 $T5))) body))))))

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
