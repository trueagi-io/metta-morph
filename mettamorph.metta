(: If (-> Bool Atom Atom))
(= (If True $then) $then)
(= (If False $then) ())
(: If (-> Bool Atom Atom Atom))
(= (If $cond $then $else) (if $cond $then $else))

(: sequential (-> Expression %Undefined%))
(= (sequential $1) (superpose $1))
