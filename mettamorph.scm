;CHICKEN
(import srfi-13) ;strings
(import amb)     ;superpose
(import amb-extras) ;superpose
(import matchable) ;let/case
(import mini-kanren) ;match

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
     (amb-collect args))))

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

(define-syntax =
  (syntax-rules ()
    ((_ (name (list $f1 $c1) (list $f2 $c2)) body) ;TODO generalize
     (define (name $T1 $T2)
        (match-let* ((($f1 $c1) $T1)
                     (($f2 $c2) $T2))
                    body)))
    ((_ (name xi ...) body)
     (define (name xi ...)
             body))))

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

(define-syntax define-atoms
  (syntax-rules ()
   ((_ argi ...)
    (list (set! argi 'argi) ...))))

(define-syntax add-atom
  (syntax-rules ()
    ((_ space (atomi ...))
     (begin
           (set! space (cons (list 'atomi ...) space))
           (define-atoms atomi ...)))))

(define === ==) ;microkanren constructs use ===
(define == =)   ;allow use == for MeTTa compatibility

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
    ((_ (expr ...))         ;that's why this construct is defined here instead
     (begin
       expr ...))))
