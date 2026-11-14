// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/commentsArgumentsOfCallExpression2.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function foo(/*c1*/ x,/*d1*/  y,/*e1*/  w) {}
var a, b;
foo(/*c2*/ 1,/*d2*/  1 + 2,/*e1*/  a + b);
foo(/*c3*/ function () {},/*d2*/  () => {},/*e2*/  a +/*e3*/  b);
foo(/*c3*/ function () {},/*d3*/  () => {},/*e3*/  (a + b));
foo(/*c4*/ function () {}, /*d4*/ () => {}, /*e4*/
/*e5*/ 'hello');