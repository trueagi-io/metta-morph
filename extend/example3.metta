!(extend-py! mettamorph)
!(compile! "
(= (f2 $x) (+ 2 $x))
(= (use1 $f $arg)
   ($f $arg))
")

(= (f1 $x) (+ 1 $x))
(= (use2 $f $arg)
   ($f $arg))

!(+ 1 (use1 f1 40))
!(+ 1 (use1 f2 39))
!(+ 1 (use2 f1 40))
!(+ 1 (use2 f2 39))
