!(extend-py! mettamorph.py)
!(compile! "
(= (facF $n)
   (If (== $n 0)
       1
       (* $n (facF (- $n 1)))))
")

!(facF 10)

