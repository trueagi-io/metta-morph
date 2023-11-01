;; Run with CHICKEN Scheme!
(import srfi-1 srfi-69)    ;filter, hashtable
(import amb amb-extras)    ;amb to implement superpose and amb1 to implements its nesting behavior
(import matchable)         ;let/case constructs as match-lambda with deconstruction
(import (chicken flonum))  ;floating point options
(import (chicken type))    ;type system

;; COLLAPSE AND SUPERPOSE
;""""""""""""""""""""""""

;collapse: using Scheme's 'amb-collect'
(define-syntax collapse
  (syntax-rules ()
    ((_ args)
     (filter (lambda (x) (not (== x (if #f 42))))
             (amb-collect (handle-exceptions exn ((amb-failure-continuation)) args))))))

;superpose-helper enforces nested superpose compatibility
;by flatting out the nested superpose calls before passing it to amb
(define-syntax superpose-helper
  (syntax-rules (superpose)
    ((_ (superpose (argi ...)))
     (amb (superpose-helper argi) ...))
    ((_ arg)
     (auto-list1 arg))))

(define-syntax superpose
  (syntax-rules ()
    ((_ (argi ...))
     (amb (superpose-helper argi) ...))
    ((_ args)
     (amb1 (superpose-helper args)))))

;; FUNCTION DEFINITION IN METTA
;""""""""""""""""""""""""""""""

;A hash map where the functions are keyed by their name.
;The values are the lists containing all functions of same name.
(define functions (make-hash-table))

;The function is added to the &self space
;then it is added to the list of functions of same name in the hashmap,
;whereby function call is carried out by executing (with backtracking)
;all registered functions of that name.
(define-syntax =
  (syntax-rules ()
    ((_ (name patterni ...) body)
     (begin
       (hash-table-set! vars '&self (cons '(=def (name patterni ...) body) (hash-table-ref vars '&self)))
       (let ((function (match-lambda* ((patterni ...) (auto-list1 body)))))
            (if (hash-table-exists? functions 'name)
                (hash-table-set! functions 'name (cons function (hash-table-ref functions 'name)))
                (hash-table-set! functions 'name (list function))))
       (set! name (lambda args (handle-exceptions exn ((amb-failure-continuation))
                          (apply (amb1 (hash-table-ref functions 'name)) args))))))))

;; SYNTACTIC CONSTRUCTS FOR DEFINING VARIABLES
;"""""""""""""""""""""""""""""""""""""""""""""

;Using Scheme's 'match-let*', while omiting the need for double-parenthesis
(define-syntax Let
  (syntax-rules ()
    ((_ var val body)
     (match-let* ((var (auto-list1 val))) (auto-list1 body)))))

;Simply map to 'match-let*'
(define-syntax Let*
  (syntax-rules ()
    ((_ ((vari vali) ...) body)
     (match-let* ((vari (auto-list1 vali)) ...) (auto-list1 body)))
    ((_ (((vari1 vari2) vali) ...) body)
     (match-let* (((vari1 vari2) (auto-list1 vali)) ...) (auto-list1 body)))))

;; SYNTACTIC CONSTRUCTS FOR CHECKING VARIABLES
;"""""""""""""""""""""""""""""""""""""""""""""

;The if-then case causes backtracking in the else branch instead of returning #undefined
(define-syntax If
  (syntax-rules ()
    ((_ condition thenbody elsebody)
        (if condition (auto-list1 thenbody) (auto-list1 elsebody)))
    ((_ condition thenbody)
        (if condition (auto-list1 thenbody) ((amb-failure-continuation))))))

;Since case causes an exception when no case is met, we catch it and backtrack instead
;additionally we handle %void% case by considering the amount of matched options
;either returning the voidcase if there was no match, or nondeterministically the matched options
(define-syntax Case
  (syntax-rules (%void%)
    ((_ var ((%void% voidcase)))
     (if (eq? 0 (length (amb-collect (auto-list1 var)))) voidcase ((amb-failure-continuation))))
    ((_ var ((pati bodi) ... (%void% voidcase)))
     (let ((options (amb-collect (handle-exceptions exn ((amb-failure-continuation))
                                                    (match (auto-list1 var) (pati (auto-list1 bodi)) ...)))))
          (if (eq? 0 (length options))
              voidcase (amb1 options))))
    ((_ var ((pati bodi) ...))
     (handle-exceptions exn ((amb-failure-continuation))
                        (match (auto-list1 var) (pati (auto-list1 bodi)) ...)))))

;; QUERY EXECUTION OPERATOR
;;"""""""""""""""""""""""""

;Printing all solutions in the list
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

;Print all solutions after collecting them
(define-syntax !
  (syntax-rules ()
    ((_ argi ...)
     (print-helper (amb-collect (auto-list argi ...))))))

;; AUTO-LIST TO ELIMINATE THE NEED FOR SYNTACTIC LIST/FUNCTIONCALL DISTINCTION
;"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

;To avoid exponential expansion of a code tree with mostly unreachable options
;we use this specialied macro instead of 'if'. Hereby, list/function call resolving is skipped for
;syntactic constructs as they represent neither a function call nor a list.
(define-syntax metta-macro-if
  (syntax-rules (collapse superpose Let Let* Match Case If == sequential quote do trace! and or)
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
    ((_ do then else) then)
    ((_ trace! then else) then)
    ((_ and then else) then)
    ((_ or then else) then)
    ((_ arg then else) else)))

;Recursively, returns a list if arg is a list or executes a function if arg is a function
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

;Recursively, resolves whether it is a syntactic construct or whether to apply list/function call distinction
(define-syntax auto-list
  (syntax-rules ()
    ((_ expr)
     (list (auto-list1 expr)))
    ((_ expr1 expri ...)
     (metta-macro-if expr1
         (expr1 expri ...)
         (auto-list-helper expr1 expri ...)))))

;same as before but on a single-arg nested expression
(define-syntax auto-list1
   (syntax-rules ()
    ((_ (vari ...))
     (auto-list vari ...))
    ((_ var1)
     var1)))

;; EQUALITY
;""""""""""

;allow using '==' in the code
(define == equal?)

;; TYPE SYSTEM
;"""""""""""""

;Type Definitions
(define-type Atom *)
(define-type %Undefined% *)
(define-type Symbol symbol)
(define-type Expression list)
(define-type Bool boolean)
(define-type Number number)
(define-type String string)

;Letting compiler know of types as well, for potential increased efficiency
(define-syntax Typedef
  (syntax-rules ()
    ((_ arg (-> A ... B))
     (cond-expand
       (USE_TYPES (: arg (A ... -> B))) (else '())))
    ((_ arg1 arg2)
     (cond-expand
       (USE_TYPES (define-type arg1 arg2)) (else '())))))

;; SPACES IMPLEMENTATION
;""""""""""""""""""""""""

;Where all state variables and spaces live, including &self
(define vars (make-hash-table))
(hash-table-set! vars '&self '())

;compatibility wrapper to construct states/spaces and retrieving state
(define (new-space s) s)
(define (new-state s) s)
(define (get-state s) s)

;get-atom returns non-deterministally all items of space s
(define (get-atoms space) (amb1 (hash-table-ref vars space)))

;bind a new space or state to a symbol var
(define (bind! var val)
  (begin (hash-table-set! vars var val) '()))

;add an atom to an existing space within the vars structure
(define (add-atom space atom)
  (begin (hash-table-set! vars space (cons atom (hash-table-ref vars space))) '()))

;remove an atom from an existing space within the vars structure
(define (remove-atom space atom)
  (begin (hash-table-set! vars space (delete atom (hash-table-ref vars space))) '()))

;change an existing state variable within the vars structure
(define (change-state! var val)
  (begin (hash-table-set! vars var val) (list 'State val)))

;retrieve the state of an existing state variable within the vars structure
(define (get-state var)
  (hash-table-ref vars var))

;match expression: using Scheme's match-let*
(define-syntax Match
  (syntax-rules (MatchChain)
    ((_ space (MatchChain bind1 bind2) result)
     (Match space bind1 (Match space bind2 result)))
    ((_ space (MatchChain bind1 bindi ...) result)
     (Match space bind1 (Match space (MatchChain bindi ...) result)))
    ((_ space binds result)
     (handle-exceptions exn ((amb-failure-continuation))
                        (match-let* ((binds (amb1 (hash-table-ref vars space)))) (auto-list1 result))))))

;; PROCEDURAL CONSTRUCTS
;"""""""""""""""""""""""

;Append expression return value to list unless it is marked to be omitted via 'do'
(define-syntax sequential-helper
  (syntax-rules (do)
    ((_ (do expr))
     expr)
    ((_ expr)
     (set! ret (append ret (list expr))))))

;procedural sequential execution can be superpose here as well
(define-syntax sequential
  (syntax-rules ()
    ((_ arg)
     (superpose arg))))

;standalone do also can cause side effects but returns no result
(define-syntax do
  (syntax-rules ()
    ((_ arg)
     (begin (auto-list1 arg) (If #f 42)))))

;; TRACE
;"""""""

;injected debug output for returned expression
(define-syntax trace!
  (syntax-rules ()
    ((_ x y)
     (begin (display (auto-list1 x)) (newline) (auto-list1 y)))))
