// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/nestedRecursiveLambda.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

function f(a:any) {
void (r =>(r => r));
}
f((r =>(r => r)));
void(r =>(r => r));
[(r =>(r => r))]