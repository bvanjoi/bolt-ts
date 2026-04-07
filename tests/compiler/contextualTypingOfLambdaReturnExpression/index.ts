// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/contextualTypingOfLambdaReturnExpression.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

function callb(lam: (l: number) => void);
function callb(lam: (n: string) => void);
function callb(a) { }

callb((a) => a.length); // Ok, we choose the second overload because the first one gave us an error when trying to resolve the lambda return type
//~^ ERROR: Property 'length' does not exist on type 'number'.
callb((a) => { a.length; }); // Error, we picked the first overload and errored when type checking the lambda body
//~^ ERROR: Property 'length' does not exist on type 'number'.
