Note that Clojure doesn't have the concept of pairs.
I've used lists of two elements instead throughout, with car and cdr replaced by first and second.
This works fine, since as 2.1.3 points out, all we need for cons, car and cdr are functions such that (car (cons x y)) is x, (cdr (cons x y)) is y,
and most of the chapter is about creating abstractions so that you don't have to worry about implementation details.

It does occasionally lead to a bit of cognitive dissonance though, when we compare the implementation using pairs to that using lists (as in 2.29d).
In these cases I've used the functional definitions of cons, car and cdr defined in ex 2.4
