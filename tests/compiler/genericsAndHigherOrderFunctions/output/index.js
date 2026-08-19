var combine = (f) => ((g) => ((x) => (f(g(x)))));
var foo = (g) => ((h) => ((f) => (h(combine(f)(g)))));