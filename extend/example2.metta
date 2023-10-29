!(import! &self ./../mettamorph.metta)
!(extend-py! mettamorph)

!(compile! "
(= (facF $n)
   (If (== $n 0)
       1
       (* $n (facF (- $n 1)))))
")

!(mettamorph facF 42)
