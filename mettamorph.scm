;run with CHICKEN Scheme!
(import (chicken condition)) ;exceptional chicken
(import srfi-1) ;filter
(import srfi-13) ;string support in Scheme
(import srfi-69) ;hashtable support in Scheme
(import amb)     ;amb to implement superpose nesting behavior
(import amb-extras) ;amb1 to implement superpose
(import matchable) ;let/case constructs with list deconstruction
(import mini-kanren) ;match with true unification
(import (chicken string)) ;->string function to convert scheme expressions to string

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
     (filter notUnspecified (amb-collect args)))))

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

(define (token-contains contained token)
    (string-contains (->string token) contained))

(define-syntax define-atoms
  (syntax-rules ()
    ((_ argi ...)
     (list (if (not (token-contains "$" 'argi)) (set! argi 'argi)) ...))))

(define functions (make-hash-table))

(define-syntax define-partial
  (syntax-rules ()
    ((_ (name xi ...) body) ;normal function definition with flattened params
     (begin (define (name xi ...) (handle-exceptions exn '() body))
            (if (hash-table-exists? functions 'name)
                (hash-table-set! functions 'name (cons name (hash-table-ref functions 'name)))
                (hash-table-set! functions 'name (list name)))
            (define (name xi ...) ((amb1 (hash-table-ref functions 'name)) xi ...))))))

(define-syntax =
  (syntax-rules () ;hard to generalize further but sufficiently powerful already
    ((_ (name (list args1 ...)) body) ;deconstruct 1 list argument
     (begin (define-partial (name $T1)
                            (match-let* (((args1 ...) $T1)) body))
            (define-atoms args1 ...)))
    ((_ (name (list args1 ...) (list args2 ...)) body) ;deconstruct 2 list arguments
     (begin (define-partial (name $T1 $T2)
                            (match-let* ((((args1 ...) (args2 ...)) (list $T1 $T2))) body))
            (define-atoms args1 ... args2 ...)))
    ((_ (name (list args1 ...) (list args2 ...) (list args3 ...)) body) ;deconstruct 3 list arguments
     (begin (define-partial (name $T1 $T2 $T3)
                            (match-let* ((((args1 ...) (args2 ...) (args3 ...)) (list $T1 $T2 $T3))) body))
            (define-atoms args1 ... args2 ... args3 ...)))
    ((_ (name (list args1 ...) xi ...) body) ;deconstruct 1 list argument with params
     (begin (define-partial (name $T1 xi ...)
                            (match-let* (((args1 ...) $T1)) body))
            (define-atoms args1 ... xi ...)))
    ((_ (name (list args1 ...) (list args2 ...) xi ...) body) ;deconstruct 2 list arguments with params
     (begin (define-partial (name $T1 $T2 xi ...)
                            (match-let* ((((args1 ...) (args2 ...)) (list $T1 $T2))) body))
            (define-atoms args1 ... args2 ... xi ...)))
    ((_ (name (list args1 ...) (list args2 ...) (list args3 ...) xi ...) body) ;deconstruct 3 list arguments with params
     (begin (define-partial (name $T1 $T2 $T3 xi ...)
                            (match-let* ((((args1 ...) (args2 ...) (args3 ...)) (list $T1 $T2 $T3))) body))
            (define-atoms args1 ... args2 ... args3 ... xi ...)))
    ((_ (name xi ...) body) ;normal function definition with flattened params
           (begin (define-partial (name xi ...) body)
                  (define-atoms xi ...)))))

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
    ((_ key
        ((else result1 result2 ...)))
     (begin result1 result2 ...))
    ((_ key
        ((atom result1 result2 ...)))
     (if (eqv? key atom)
         (begin result1 result2 ...)))
    ((_ key
        ((atom result1 result2 ...)
         clause clauses ...))
     (if (eqv? key atom)
         (begin result1 result2 ...)
         (CaseMetta key (clause clauses ...))))))

(define &self '())

(define-syntax add-atom
  (syntax-rules ()
    ((_ space (atomi ...))
     (begin
           (set! space (cons (list 'atomi ...) space))
           (define-atoms atomi ...)))))

(define === ==) ;microkanren constructs use ===
(define == equal?)   ;allow use == for MeTTa compatibility

(define (conso x L Lx)
        (=== Lx (cons x L)))

(define (symbolo s)
        (lambda (s/c)
                (let ((s (walk s (car s/c))))
                     (if (or (symbol? s) (list? s)) ;added: list
                         (unit s/c)
                         mzero))))

(define (firstK number lst)
        (if (equal? 0 number)
            '()
            (cons (car lst)
                  (firstK (- number 1) (cdr lst)))))

(define (var-to-number str)
        (string->number (string-drop (symbol->string str) 1)))

(define-syntax MatchMetta
  (syntax-rules ()
    ((_ space (pati ...) (reti ...))
     (let* ((counter1 -1)
            (counter2 -1)
            (refcounter -1)
            (varindices '())
            (value (amb1 (run* (Q)
                               (fresh ($0 $1 $2 $3 $4 $5 $6 $7 $8 $9 $10)
                                      (let ((vars (list $0 $1 $2 $3 $4 $5 $6 $7 $8 $9 $10)))
                                           (fresh (KB)
                                                  (conso (list (if (string-prefix? "$" (symbol->string 'pati))
                                                                   (begin (set! counter1 (+ counter1 1))
                                                                          (set! varindices (cons (cons (symbol->string 'pati) counter1) varindices))
                                                                          (list-ref vars counter1)) pati) ...) space KB)
                                                  (membero (list (if (string-prefix? "$" (symbol->string 'pati))
                                                                     (begin (set! counter2 (+ counter2 1))
                                                                            (list-ref vars counter2)) pati) ...) KB)
                                                  (=== Q (firstK (+ 1 counter2) vars))
                                                  (symbolo (list-ref vars counter2)))))))))
            (list (if (string-prefix? "$" (symbol->string 'reti))
                      (list-ref value (cdr (assoc (symbol->string 'reti) varindices))) reti) ...)))
    ((_ space (pati ...) ret)
     (let ((value (amb1 (run* (Q)
                      (fresh (KB)
                             (conso (list (if (string-prefix? "$" (symbol->string 'pati)) Q pati) ...) space KB)
                             (membero (list (if (string-prefix? "$" (symbol->string 'pati)) Q pati) ...) KB))
                             (symbolo Q)))))
                value))))

(define-syntax sequential ;sequential cannot be superpose in Scheme as in MeTTa
  (syntax-rules ()        ;as procedural sequential execution demands :begin"
    ((_ (expr ...))       ;that's why this construct is defined here instead
     (begin
       expr ...))))
