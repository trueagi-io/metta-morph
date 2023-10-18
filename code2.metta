(= (TupleConcat $Ev1 $Ev2) (collapse (superpose (list (superpose $Ev1) (superpose $Ev2)))))

;; Truth functions
(= (Truth_c2w $c) (/ $c (- 1 $c)))
(= (Truth_w2c $w) (/ $w (+ $w 1)))
(= (Truth_Deduction ($f1 $c1) ($f2 $c2)) (list (* $f1 $f2) (* (* $f1 $f2) (* $c1 $c2))))
(= (Truth_Abduction ($f1 $c1) ($f2 $c2)) (list $f2 (Truth_w2c (* (* $f1 $c1) $c2))))
(= (Truth_Induction $T1 $T2) (Truth_Abduction $T2 $T1))
(= (Truth_Revision ($f1 $c1) ($f2 $c2))
   (let* (($w1 (Truth_c2w $c1)) ($w2 (Truth_c2w $c2)) ($w  (+ $w1 $w2))
          ($f (/ (+ (* $w1 $f1) (* $w2 $f2)) $w)) ($c (Truth_w2c $w)))
          (list (min 1.00 $f) (min 0.99 (max (max $c $c1) $c2)))))

;; Inference rules
; NAL-1/5 (selected subset)
(= (|- ($T $T1) ($T $T2)) (list $T (Truth_Revision $T1 $T2)))
(= (|- (($a '--> $b) $T1) (($b '--> $c) $T2)) ((list $a '--> $c) (Truth_Deduction $T1 $T2)))
(= (|- (($a '--> $b) $T1) (($a '--> $c) $T2)) ((list $c '--> $b) (Truth_Induction $T1 $T2)))
(= (|- (($a '--> $c) $T1) (($b '--> $c) $T2)) ((list $b '--> $a) (Truth_Abduction $T1 $T2)))
(= (|- (($a '==> $b) $T1) (($b '==> $c) $T2)) ((list $a '==> $c) (Truth_Deduction $T1 $T2)))
(= (|- (($a '==> $b) $T1) (($a '==> $c) $T2)) ((list $c '==> $b) (Truth_Induction $T1 $T2)))
(= (|- (($a '==> $c) $T1) (($b '==> $c) $T2)) ((list $b '==> $a) (Truth_Abduction $T1 $T2)))
(= (|- ($A $T1) (($A '==> $B) $T2)) (list $B (Truth_Deduction $T1 $T2)))
(= (|- ($A $T1) ((($A '&& $B) '==> $C) $T2)) ((list $B '==> $C) (Truth_Deduction $T1 $T2)))
(= (|- ($B $T1) (($A '==> $B) $T2)) (list $A (Truth_Abduction $T1 $T2)))
; NAL-4 (selected subset)
(= (|- ((($A 'x $B) '--> $R) $T1) ((($C 'x $B) '--> $R) $T2)) (list (list $C '--> $A) (Truth_Abduction $T1 $T2)))
(= (|- ((($A 'x $B) '--> $R) $T1) ((($A 'x $C) '--> $R) $T2)) (list (list $C '--> $B) (Truth_Abduction $T1 $T2)))
(= (|- (($R '--> ($A 'x $B)) $T1) (($R '--> ($C 'x $B)) $T2)) (list (list $C '--> $A) (Truth_Induction $T1 $T2)))
(= (|- (($R '--> ($A 'x $B)) $T1) (($R '--> ($A 'x $C)) $T2)) (list (list $C '--> $B) (Truth_Induction $T1 $T2)))
(= (|- ((($A 'x $B) '--> $R) $T1) (($C '--> $A) $T2)) (list (list (list $C 'x $B) '--> $R) (Truth_Deduction $T1 $T2)))
(= (|- ((($A 'x $B) '--> $R) $T1) (($A '--> $C) $T2)) (list (list (list $C 'x $B) '--> $R) (Truth_Induction $T1 $T2)))
(= (|- ((($A 'x $B) '--> $R) $T1) (($C '--> $B) $T2)) (list (list (list $A 'x $C) '--> $R) (Truth_Deduction $T1 $T2)))
(= (|- ((($A 'x $B) '--> $R) $T1) (($B '--> $C) $T2)) (list (list (list $A 'x $C) '--> $R) (Truth_Induction $T1 $T2)))
(= (|- (($R '--> ($A 'x $B)) $T1) (($A '--> $C) $T2)) (list (list $R '--> (list $C 'x $B)) (Truth_Deduction $T1 $T2)))
(= (|- (($R '--> ($A 'x $B)) $T1) (($C '--> $A) $T2)) (list (list $R '--> (list $C 'x $B)) (Truth_Abduction $T1 $T2)))
(= (|- (($R '--> ($A 'x $B)) $T1) (($B '--> $C) $T2)) (list (list $R '--> (list $A 'x $C)) (Truth_Deduction $T1 $T2)))
(= (|- (($R '--> ($A 'x $B)) $T1) (($C '--> $B) $T2)) (list (list $R '--> (list $A 'x $C)) (Truth_Abduction $T1 $T2)))

;Whether evidence was just counted once
(= (StampDisjoint $Ev1 $Ev2)
   (== () (collapse (let* (($x (superpose $Ev1))
                           ($y (superpose $Ev2)))
                          (case (== $x $y) ((True 'overlap)))))))

;; Exhaustive-until-depth deriver
(= (Derive $beliefs $depth $maxdepth)
   (If (> $depth $maxdepth)
       $beliefs
       (let $derivations 
            (collapse (let* ((('Sentence $x $Ev1) (superpose $beliefs))
                             (('Sentence $y $Ev2) (superpose $beliefs))
                             ($stamp (TupleConcat $Ev1 $Ev2)))
                            (If (StampDisjoint $Ev1 $Ev2) 
                                (case (|- $x $y) ((($T $TV) (list 'Sentence (list $T $TV) $stamp)))))))
            (Derive (TupleConcat $beliefs $derivations) (+ $depth 1) $maxdepth))))

;retrieve the best candidate
(= (BestCandidate $evaluateCandidateFunction $bestCandidate $tuple)
  (If (== $tuple ())
      $bestCandidate
      (let* (($head (car-atom $tuple))
             ($tail (cdr-atom $tuple)))
            (If (> ($evaluateCandidateFunction $head)
                   ($evaluateCandidateFunction $bestCandidate))
                (BestCandidate $evaluateCandidateFunction $head $tail)
                (BestCandidate $evaluateCandidateFunction $bestCandidate $tail)))))

;candidate evaluation based on confidence
(= (ConfidenceRank (($f $c) $Ev)) $c)
(= (ConfidenceRank ()) 0)
;(= (ConfidenceRank $List)
;   (if (== $List Nil)
;       0
;       (cdr-atom (cdr-atom $List))))

;pose a question of a certain term to the system on some knowledge base
(= (Question $kb $term $steps)
   (BestCandidate ConfidenceRank () (collapse (let $x (Derive $kb 1 $steps) 
                                                   (case (superpose $x)
                                                         ((('Sentence ($T $TV) $Ev) (case (== $T $term)
                                                                                          ((True (list $TV $Ev)))))))))))


;<EVALUATIONS>

;(Question (list (list 'Sentence (list (list 'a '--> 'b) (list 1.0 0.9)) (list 1))
;                (list 'Sentence (list (list 'b '--> 'c) (list 1.0 0.9)) (list 2))
;                (list 'Sentence (list (list 'c '--> 'd) (list 1.0 0.9)) (list 3))
;                (list 'Sentence (list (list 'd '--> 'e) (list 1.0 0.9)) (list 4))
;                (list 'Sentence (list (list 'e '--> 'f) (list 1.0 0.9)) (list 5))
;          )
;          (list 'a '--> 'f) 3)

;(BestCandidate ConfidenceRank Nil (list (list (list 1.0 0.9) (list 0)) 
;                                        (list (list 1.0 0.9) (list 0))
;                                        (list (list 1.0 0.9) (list 0))
;                                        (list (list 1.0 0.9) (list 0))
;                                        (list (list 1.0 0.9) (list 0))
;                                        (list (list 1.0 0.9) (list 0))
;                                        (list (list 1.0 0.9) (list 0))
;                                        (list (list 1.0 0.9) (list 0))
;                                        (list (list 1.0 0.9) (list 0))))

;(ConfidenceRank (list (list 1.0 0.9) (list 0)))
;(define Nil '())
;(define Nil '())
;(ConfidenceRank Nil)

;(Derive (list (list Sentence (list (list garfield --> cat) (list 1.0 0.9)) (list 1)) 
;              (list Sentence (list (list cat --> blue) (list 1.0 0.9)) (list 2))
;              ;(list Sentence (list (list dog --> blue) (list 1.0 0.9)) (list 3))
;              ) 1 
;              4)
              
;(let $beliefs (list (list Sentence (list (list garfield --> cat) (list 1.0 0.9)) (list 1)) 
;              (list Sentence (list (list cat --> blue) (list 1.0 0.9)) (list 2)))              
;              (collapse (let* (((Sentence $x $Ev1) (superpose $beliefs))
;                             ((Sentence $y $Ev2) (superpose $beliefs)) ;issue here
;                             ($stamp (TupleConcat $Ev1 $Ev2)))
;                             (If (StampDisjoint $Ev1 $Ev2) 
;                                 (case (|- $x $y) 
;                                       ((($T $TV) 
;                                         (list Sentence (list $T $TV) $stamp)))) )
;                              )))


;(|- (list (list 'a '==> 'b) (list 1.0 0.9)) (list (list 'b '==> 'c) (list 1.0 0.9)))


;(case (|- (list (list a ==> b) (list 1.0 0.9)) (list (list b ==> c) (list 1.0 0.9)))
;      (($else $else)))


;(Question (list (list 'Sentence (list (list 'garfield '--> 'cat) (list 1.0 0.9)) (list 1)) 
;                (list 'Sentence (list (list 'cat '--> 'blue) (list 1.0 0.9)) (list 2)))
;          (list 'garfield '--> 'blue) 1)

;(Question ((list Sentence (list (({ garfield }) --> cat) (list 1.0 0.9)) (list 1)) 
;            (list Sentence (list ((cat * sky) --> like) (list 1.0 0.9)) (list 2)) 
;            (list Sentence (list (sky --> ([ blue ])) (list 1.0 0.9)) (list 3))
;            (list Sentence (list (((({ garfield }) * ([ blue ])) --> like) ==> (({ garfield }) --> artist)) (list 1.0 0.9)) (list 4)))
;           (list ({ garfield }) --> artist) 3)


;(Question ((list Sentence (list garfield  --> cat) (list 1.0 0.9) (list 1)) 
;            (list Sentence (list (list (cat * sky) --> like) (list 1.0 0.9)) (list 2)) 
;            (list Sentence (list (sky --> blue) (list 1.0 0.9)) (list 3))
;            (list Sentence (list ((( garfield  * blue ) --> like) ==> ( garfield  --> artist)) (list 1.0 0.9)) (list 4)))
;           (list ({ garfield }) --> artist) 3)


(Question (list (list 'Sentence (list (list (list 'ExtSet 'garfield) '--> 'cat) (list 1.0 0.9)) (list 1)) 
                (list 'Sentence (list (list (list 'cat 'x 'sky) '--> 'like) (list 1.0 0.9)) (list 2))
                (list 'Sentence (list (list 'sky '--> (list 'IntSet 'blue)) (list 1.0 0.9)) (list 3))
                (list 'Sentence (list (list (list (list (list 'ExtSet 'garfield) 'x (list 'IntSet 'blue)) '--> 'like) '==> (list (list 'ExtSet 'garfield) '--> 'artist)) (list 1.0 0.9)) (list 4))
          )
          ;(list (list (list ExtSet garfield) x (list IntSet blue)) --> like)
          (list (list 'ExtSet 'garfield) '--> 'artist)
          3)


;(superpose (list 1 1))

;(== 1 1)

;(StampDisjoint (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20) (list 991 992 993 994 995 996 997 998 999 9910 9911 9912 9913 9914 9915 9916 9917 9918 9919 9920))

;(Truth_Revision (list 1.0 0.9) (list 1.0 0.9))

;(collapse (let* (($Ev1 (list 1 2 3))
;       ($Ev2 (list 3 5 6)))
;    (let* (($x (superpose $Ev1))
;           ($y (superpose $Ev2)))
 ;          (case (== $x $y) ((True 42) )))))


;(collapse (let $x (superpose (list 10 4))
;                  (case (== $x 4) ((True 42)))))

;(= (|- ($T $T1) ($T $T2)) ($T (Truth_Revision $T1 $T2)))

;(|- (list (list a ==> b) (list 1.0 0.9)) (list (list b ==> c) (list 1.0 0.9)))
