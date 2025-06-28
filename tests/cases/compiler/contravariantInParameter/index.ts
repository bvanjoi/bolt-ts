interface N {
    a: number
}
function k(_f: (a: N) => void) {}

interface M extends N {
    b: number
}
function g(b: M) {}
k(g)
//~^ ERROR: Argument of type '(b: M) => void' is not assignable to parameter of type '(a: N) => void'.