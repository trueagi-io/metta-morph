(= (TupleConcat $Ev1 $Ev2) (collapse (superpose (list (superpose $Ev1) (superpose $Ev2)))))

;; Truth functions
(= (Truth_c2w $c) (/ $c (- 1 $c)))
(= (Truth_w2c $w) (/ $w (+ $w 1)))
(= (Truth_Deduction (list $f1 $c1) (list $f2 $c2)) (list (* $f1 $f2) (* (* $f1 $f2) (* $c1 $c2))))
(= (Truth_Abduction (list $f1 $c1) (list $f2 $c2)) (list $f2 (Truth_w2c (* (* $f1 $c1) $c2))))
(= (Truth_Induction $T1 $T2) (Truth_Abduction $T2 $T1))
(= (Truth_Revision (list $f1 $c1) (list $f2 $c2))
   (let* (($w1 (Truth_c2w $c1)) ($w2 (Truth_c2w $c2)) ($w  (+ $w1 $w2))
          ($f (/ (+ (* $w1 $f1) (* $w2 $f2)) $w)) ($c (Truth_w2c $w)))
          (list (min 1.00 $f) (min 0.99 (max (max $c $c1) $c2)))))

(define-atoms a b c --> ==> x Sentence garfield sky blue cat dog)
;; Inference rules
; NAL-1/5 (selected subset)
(= (|- (list $T $T1) (list $T $T2)) (list $T (Truth_Revision $T1 $T2)))
(= (|- (list ($a $-->/==> $b) $T1) (list ($b $-->/==> $c) $T2)) (list (list $a $-->/==> $c) (Truth_Deduction $T1 $T2)))
(= (|- (list ($a $-->/==> $b) $T1) (list ($a $-->/==> $c) $T2)) (list (list $c $-->/==> $b) (Truth_Induction $T1 $T2)))
(= (|- (list ($a $-->/==> $c) $T1) (list ($b $-->/==> $c) $T2)) (list (list $b $-->/==> $a) (Truth_Abduction $T1 $T2)))
(= (|- (list $A $T1) (list ($A ==> $B) $T2)) (list $B (Truth_Deduction $T1 $T2)))
(= (|- (list $A $T1) (list (($A && $B) ==> $C) $T2)) (list (list $B ==> $C) (Truth_Deduction $T1 $T2)))
(= (|- (list $B $T1) (list ($A ==> $B) $T2)) (list $A (Truth_Abduction $T1 $T2)))
; NAL-4 (selected subset)
(= (|- ((($A x $B) --> $R) $T1) ((($C x $B) --> $R) $T2)) (($C --> $A) (Truth_Abduction $T1 $T2)))
(= (|- ((($A x $B) --> $R) $T1) ((($A x $C) --> $R) $T2)) (($C --> $B) (Truth_Abduction $T1 $T2)))
(= (|- (($R --> ($A x $B)) $T1) (($R --> ($C x $B)) $T2)) (($C --> $A) (Truth_Induction $T1 $T2)))
(= (|- (($R --> ($A x $B)) $T1) (($R --> ($A x $C)) $T2)) (($C --> $B) (Truth_Induction $T1 $T2)))
(= (|- ((($A x $B) --> $R) $T1) (($C --> $A) $T2)) ((($C x $B) --> $R) (Truth_Deduction $T1 $T2)))
(= (|- ((($A x $B) --> $R) $T1) (($A --> $C) $T2)) ((($C x $B) --> $R) (Truth_Induction $T1 $T2)))
(= (|- ((($A x $B) --> $R) $T1) (($C --> $B) $T2)) ((($A x $C) --> $R) (Truth_Deduction $T1 $T2)))
(= (|- ((($A x $B) --> $R) $T1) (($B --> $C) $T2)) ((($A x $C) --> $R) (Truth_Induction $T1 $T2)))
(= (|- (($R --> ($A x $B)) $T1) (($A --> $C) $T2)) (($R --> ($C x $B)) (Truth_Deduction $T1 $T2)))
(= (|- (($R --> ($A x $B)) $T1) (($C --> $A) $T2)) (($R --> ($C x $B)) (Truth_Abduction $T1 $T2)))
(= (|- (($R --> ($A x $B)) $T1) (($B --> $C) $T2)) (($R --> ($A x $C)) (Truth_Deduction $T1 $T2)))
(= (|- (($R --> ($A x $B)) $T1) (($C --> $B) $T2)) (($R --> ($A x $C)) (Truth_Abduction $T1 $T2)))

;Whether evidence was just counted once
(= (StampDisjoint $Ev1 $Ev2)
   (== () (collapse (let* (($x (superpose $Ev1))
                           ($y (superpose $Ev2)))
                          (case (== $x $y) ((True 42)))))))

;; Exhaustive-until-depth deriver
(= (Derive $beliefs $depth $maxdepth)
   (if (> $depth $maxdepth)
       $beliefs
       (let $derivations 
            (collapse (let* (((Sentence $x $Ev1) (superpose $beliefs))
                             ((Sentence $y $Ev2) (superpose $beliefs))
                             ($stamp (TupleConcat $Ev1 $Ev2)))
                            (If (StampDisjoint $Ev1 $Ev2) 
                                (case (|- $x $y) ((($T $TV) (list Sentence (list $T $TV) $stamp)))))))
            (Derive (TupleConcat $beliefs $derivations) (+ $depth 1) $maxdepth))))


;; Exhaustive-until-depth deriver
(= (DeriveDebug $beliefs $depth $maxdepth)
   (if (> $depth $maxdepth)
       $beliefs
       (let $derivations 
            (collapse (let* (((Sentence $x $Ev1) (superpose $beliefs))
                             )
                            42))
            $derivations)))


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
(= (ConfidenceRank (list ($f $c) $Ev)) $c)
(= (ConfidenceRank Nil) 0)
;(= (ConfidenceRank $List)
;   (if (== $List Nil)
;       0
;       (cdr-atom (cdr-atom $List))))

;pose a question of a certain term to the system on some knowledge base
(= (Question $kb $term $steps)
   (let (Sentence ($Term $Truth) $Ev) (superpose (Derive $kb 1 $steps)) 
        (if (== $Term $term)
            (list Sentence (list $Term $Truth) $Ev))))
   ;(BestCandidate ConfidenceRank Nil (collapse (Derive $kb 1 $steps))))

;<EVALUATIONS>

;(Question (list (list Sentence (list (list garfield --> cat) (list 1.0 0.9)) (list 1)) 
;                (list Sentence (list (list cat --> blue) (list 1.0 0.9)) (list 2))
;               ;(list Sentence (list (list dog --> blue) (list 1.0 0.9)) (list 3))
;          )
;          (list garfield --> blue) 1)

;(collapse (BestCandidate ConfidenceRank Nil (list (list (list 1.0 0.9) (list 0)) 
;                                        (list (list 1.0 0.9) (list 0))
;                                        (list (list 1.0 0.9) (list 0))
;                                        (list (list 1.0 0.9) (list 0))
;                                        (list (list 1.0 0.9) (list 0))
;                                        (list (list 1.0 0.9) (list 0))
;                                        (list (list 1.0 0.9) (list 0))
;                                        (list (list 1.0 0.9) (list 0))
;                                        (list (list 1.0 0.9) (list 0)))))

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

;(case (|- (list (list a ==> b) (list 1.0 0.9)) (list (list b ==> c) (list 1.0 0.9)))
;      (($else $else)))


;(Question (list (list Sentence (list (list garfield --> cat) (list 1.0 0.9)) (list 1)) 
;                (list Sentence (list (list cat --> blue) (list 1.0 0.9)) (list 2)))
;          (list garfield --> blue) 1)

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

;(define a

;(|- (list (list a ==> b) (list 1.0 0.9)) (list (list b ==> c) (list 1.0 0.9)))
