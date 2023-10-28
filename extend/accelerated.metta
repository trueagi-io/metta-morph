(= (facF $n)
   (If (== $n 0)
       1
       (* $n (facF (- $n 1)))))

(= (test1 $x) 
   'wat)

(= (test2 $x) 
   True)

(= (test3 $x) 
   "mettamorph")

(= (test4 $x) 
   ('good 'works))

(= (test5 $x) 
   ('rustmettafunc 'works))
