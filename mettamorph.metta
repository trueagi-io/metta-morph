(: If (-> Bool Atom $t))
(= (If True $then) $then)
(= (If False $then) (empty))
(: If (-> Bool Atom Atom $t))
(= (If $cond $then $else) (if $cond $then $else))

(: sequential (-> Expression %Undefined%))
(= (sequential $1) (superpose $1))

(: hyperpose (-> Expression %Undefined%))
(= (hyperpose $1) (superpose $1))

(: do (-> Expression %Undefined%))
(= (do $1) (case $1 ()))

(= (max $1 $2) (if (> $1 $2) $1 $2))
(= (min $1 $2) (if (< $1 $2) $1 $2))
(= (abs $x) (If (< $x 0) (- 0 $x) $x))
