(= (f $x) (* $x $x))

(= (wuza $x)
   (let* (($y (* 2 $x))
          ($z (superpose (list 4 5))))
          (list $y $z)))

(= (wuzaL $x $L)
   (let* (($y (* 2 $x))
          ($z (superpose $L)))
          (list $y $z)))

(= (facF $n)
   (if (== $n 0)
       1
       (* $n (facF (- $n 1)))))

(= (TupleConcat $Ev1 $Ev2) (collapse (superpose (list (superpose $Ev1) (superpose $Ev2)))))

(= (range $K $N)
   (If (< $K $N)
       (TupleConcat (list $K) (range (+ $K 1) $N))))

;(= (Truth_Deduction $T1 $T2) 
;   (let* ((($f1 $c1) $T1)
;          (($f2 $c2) $T2))
;         (list (* $f1 $f2) (* (* $f1 $f2) (* $c1 $c2)))))

(= (Truth_Deduction ($f1 $c1) ($f2 $c2))
   (list (* $f1 $f2) (* (* $f1 $f2) (* $c1 $c2))))

;(= (TemporalSequence $Ev1 (Event ($Term2 $Truth2) ($Time2 $Evidence2 $EPrio2)))
;   (let (Event ($Term1 $Truth1) ($Time1 $Evidence1 $EPrio1)) (Projection $Ev1 $Time2)
;        (Event (($Term1 &/ $Term2) (Truth_Intersection $Truth1 $Truth2)) ($Time2 (TupleConcat $Evidence1 $Evidence2) (0 0.0)))))

(= (goodif $x)
   (If (> 4 3) ($x 2) (4 5)))

(= (g $x $y) ($y $x))

(= (goodfunc $w) ($w 2 3))


;<EVALUATIONS>

;(goodif 3)

;(goodfunc f)
;(1 2 3)
;(goodfunc 42)
;(list (goodfunc 1) (goodfunc g))


;

;(superpose ((add-atom &self (man mortal)) (match &self ($Q mortal) $Q)))

;(collapse (superpose ((add-atom &self (man mortal)) (match &self ($Q mortal) $Q))))

;(sequential (
;(add-atom &self ('man2 'mortal))
;(add-atom &self ('man 'mortal))
;(match &self ($a 'mortal) (list 'mortal $a))))

;(sequential (
;(add-atom &self (man mortal))
;(add-atom &self (man2 mortal))
;(match &self ($a mortal) (mortal $a))))

;(add-atom &self (man mortal))

;(superpose (list (add-atom &self (man mortal)) (match &self ($Q mortal) $Q)))
;(let ($f $c) (list 1.0 0.9) (list $f $c))


;(let $T (list 1.0 0.9)
;    (let* ((($f1 $c1) $T))
;          (list $f1 $c1)))

;(let $T (list 1.0 0.9) (let* (($f1 $c1) (1.0 0.9)) $f1))

;(range 1 50)

;(list ;(facF 100)
;      (wuza 5)
;      (superpose (list (list 1 2)
;      (collapse (superpose (list 3 4 (f 5)))))))


;(Truth_Deduction (list 1.0 0.9) (list 1.0 0.9))

;(list 1 2)

;(wuza 5)

;(wuzaL 2 (list 3 4))

;(TupleConcat (list 1 2 3) (list 4 5 6))
;(superpose ( (superpose (list 1 2 3)) (superpose (list 4 5 6))))

;(superpose (list (superpose (list 1 2)) (superpose (list 3 4))))

;(TupleConcat (list 1 2 3) (list 4 5 6))


;(superpose (list 1 2))

;(collapse (superpose (list 1 2)))

;(facF 10)

;(let $x 3 $x)
;(let (($x 3)) $x)


;(let $x 3 (if (> $x (superpose (list 2 3))) 1 0))

;(let $x 34 (case $x
;                 ((3 42)
;                 ($else $else))))

;(let $x 34 (case $x ((3 42) 
;                     ($else (wuzaL 3 (list 1 2))))))

;(let* ((($a $b) (list 1 3))) $b)

;(let* (($a 1) ($b 3)) $b)

;(let* (($y 2)
;       ($z 1))
;          (list $y $z))

;(wuza 5)

;(wuzaL 2 (list 3 4))

;(collapse (let $c 21 (let* ((($a $b) ((if (== $c 5) 42 (superpose (list 43 44))) 3))) $a)))

;(if (== (let $c 21 (let* ((($a $b) ($c 3))) $b)) 3)
;    "yes" "no")


;(collapse (case (== 1 2) ((True 42))))
