// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/commentsArgumentsOfCallExpression1.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function foo(/*c1*/ x) {}
foo(/*c2*/ 1);
foo(/*c3*/ function () {});
foo(/*c4*/
() => {});
foo(/*c5*/
/*c6*/
() => {});
foo(/*c7*/ () => {});
foo(/*c7*/
/*c8*/ () => {});