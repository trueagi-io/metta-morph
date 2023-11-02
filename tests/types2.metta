(: Cat Atom)
(: Lion Cat)

(: reproduce (-> Lion Lion Lion))
(= (reproduce $x $y) childlion)

!(reproduce lion1 lion2)

!(get-type Lion)

!(match &self (: Lion $1) $1)
!(match &self (: reproduce $1) $1)
