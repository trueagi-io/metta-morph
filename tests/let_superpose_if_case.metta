(= (f $x) 42)

!(let $y (superpose (3 4 5))
      (If (> $y 3)
          (case (1 $y) (((1 3) (f 0))
                        ((1 4) (42 42))
                       ($else (1 1))))))
