;<EVALUATIONS>

(let $y (superpose (3 4 5))
     (If (> $y 3)
         (case (list 1 $y) (((1 3) 0)
                            ((1 4) 42)
                            ($else 1)))))
