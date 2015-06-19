Note that Clojure doesn't have the concept of pairs.
I've used lists instead throughout, with car and cdr replaced by first and second.
This works fine (since as 2.1.3 points out, all we need for cons, car and cdr are functions such that (car (cons x y)) is x, (cdr (cons x y)) is y.
It does occasionally lead to a bit of cognitive dissonance though, when we compare the implementation using pairs to that using lists (as in 2.29d)
