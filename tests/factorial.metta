(= (facF $n)
   (if (== $n 0)
       1
       (* $n (facF (- $n 1)))))

!(facF 10)
