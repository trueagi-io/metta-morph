;TODO implement standard intersection, union and unique functionality rather than cooking own version

(= (Tintersection $A $B)
   (let* (($x (superpose $A))
          ($y (superpose $B)))
         (If (== $x $y) $x)))

(= (Tunique $A)
   (If (== $A ())
       ()
       (If (== () (collapse (Tintersection ((car-atom $A)) (cdr-atom $A))))
           (TupleConcat ((car-atom $A)) (Tunique (cdr-atom $A)))
           (Tunique (cdr-atom $A)))))

(= (TuniqueUnion $A $B)
   (Tunique (TupleConcat $A $B)))
