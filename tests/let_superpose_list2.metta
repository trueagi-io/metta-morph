(= (wuzaL $x $L)
   (let* (($y (* 2 $x))
          ($z (superpose $L)))
          ($y $z)))

;<EVALUATIONS>

(wuzaL 2 (list 3 4))
