(= (wuza $x)
   (let* (($y (* 2 $x))
          ($z (superpose (list 4 5))))
          (list $y $z)))

;<EVALUATIONS>

(wuza 5)
