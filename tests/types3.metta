(GreaterThan 42 21)

(= (EqualOrGreater $x $y)
   (or (> $x $y) (== $x $y)))

!(match &self (GreaterThan $x $y)
    (If (and (== (get-type $x) Number) (== (get-type $y) Number))
        (If (EqualOrGreater $x $y) yay)))
