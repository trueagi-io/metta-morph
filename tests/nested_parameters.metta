(= (Truth_Deduction ($f1 $c1) ($f2 $c2))
   (list (* $f1 $f2) (* (* $f1 $f2) (* $c1 $c2))))

;<EVALUATIONS>

(Truth_Deduction (list 1.0 0.9) (list 1.0 0.9))
