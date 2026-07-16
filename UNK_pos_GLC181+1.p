function p q : -> formula

/* expected: UnknownStatus */
and(box(imp(box(imp(box(not(box(p()))), box(q()))), box(imp(box(not(box(q()))), box(p()))))), box(imp(box(imp(box(not(box(q()))), box(p()))), box(imp(box(not(box(p()))), box(q())))))) ;
