!(extend-py! mettamorph)
!(compile! "
;supported as in no case does this function return 2 values
;it just uses non-determinism machinery to execute, but always yields just 1 value
;as the cases are exclusive, such functions can directly be called by Mettamorph
;as they are deterministic:
(= (Add $x Z) $x)
(= (Add $x (S $y)) (Add (S $x) $y))

;This non-determinism however would need to backtrack into MeTTa
;to yield all options, which does not work,
;but with collapse it can be flattened:
(= (Sub) 2)
(= (Sub) 3)
(= (SubWrap) (collapse (Sub)))
")

!(Add (S (S (S Z))) (S (S (S Z))))
!(let $T (SubWrap) (superpose $T))
