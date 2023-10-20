(= (TupleConcat $Ev1 $Ev2) (collapse (superpose (list (superpose $Ev1) (superpose $Ev2)))))

(= (range $K $N)
   (If (< $K $N)
       (TupleConcat (list $K) (range (+ $K 1) $N))
       ()))

;<EVALUATIONS>

(range 1 10)
