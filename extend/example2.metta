!(extend-py! mettamorph)
!(compile! "
(= (facF $n)
   (If (== $n 0)
       1
       (* $n (facF (- $n 1)))))
")
!(import! &self METTAMORPH.metta)

!(facF 42)

