(= (wuzaL $x $L)
   (let* (($y (* 2 $x))
          ($z (superpose $L)))
          ($y $z)))

!(wuzaL 2 (3 4))
