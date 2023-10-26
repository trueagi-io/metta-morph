(= (wuza $x)
   (let* (($y (* 2 $x))
          ($z (superpose (4 5))))
          ($y $z)))

!(wuza 5)
