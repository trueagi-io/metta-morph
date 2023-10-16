
;(= (f $x) $x)
;(= (f $w) (* 2 $w))
;(= (f $a $b) $b)

;(= (test 3) 4)
;(= ('test 3) 4)

;(= (test1 ($b $x 'w1) ($a $x 'w2) 'test2) $x)
;(= (test1 ($b $x 'w1) ($a $x 'w2) 'test2) (* 2 $x))

;(= (wat $a 'test) $a)


(= (wat ()) 0)
(= (wat ($L $L2)) $L)


(= (test99 'test1 'test2 $a) 42)
(= (test99 'test1 'test2 $a) 43)
;(= (test99 'test1 'test2 'test4) 122)
;(= (test99 $x 'test2 'test3) $x)


;(= (test3 'wat $a) 0)
;(= (test3 ('lol $b)) $b)

;(= (G $x) $x)
;(= (G $w $x) $x)

;(= (facF $n)
;   (if (== $n 0)
;       1
;       (* $n (facF (- $n 1)))))

;(= (watwat ($a $b)) (+ $a $b))


;(define test "test")
;<EVALUATIONS>

(test99 'test1 'test2 'test3)

;(wat (list 3 4))

;(wat 42 'test)

;(test99 'test1 'test2 'test3)
;(test1 (list 1 2 'w1) (list 99 2 'w2) 'test2)

;(facF 3)

;(watwat (list 1 2))
;(G 1 2)
;(test1 'k (list 111 43 'w1) (list 42 43 'w2) 'test2)
;(test2 (list 'lol 42))
;(+ 1 1)

;language difference, beware of redefining vars locally which is fine in scheme
;while in metta they behave like logical variables
;which will violate equal-to-previous-assigned-value constraint.
;(let $x (let $x 3 $x) (let $x 4 $x))

;beware of difference of non-constants
;(let ($x test) (list 1 2) $x)
;(let ($x y) (list 1 2) $x)
;use $ for variables and ' for atoms, then scheme and metta are compatible,
;plus list whenever function evaluation is not desired, and all will be good:
;(let ($x 'test) (list 1 'test) $x)

;(let ($x $x) (list 1 1) $x)

;(test 3)

;(let ($x 'test) (list 1 'test) $x)

;use ' for atoms for now until this 
;(let ($x test) (list 1 2) $x)

;(f 1)
;(display functions)
;(let ((name 'test))
;     (string->symbol (string-append (symbol->string name) "1")))
;('test)

;(sequential (
;(add-atom &self ('man 'mortal))
;;(add-atom &self ('man2 'mortal))
;;this would only work in scheme:
;;(let ($a 'mortal) (superpose &self) (list 'mortal $a))))
;(match &self ($a 'mortal) (list 'mortal $a))
;;(let binds (amb1 space) result)
;))
;;))

