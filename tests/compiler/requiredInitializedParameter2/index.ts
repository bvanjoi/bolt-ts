// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/requiredInitializedParameter2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

interface I1 {
    method();
}

class C1 implements I1 {
    method(a = 0, b) { }
    //~^ ERROR: Property 'method' in type 'C1<C1>' is not assignable to the same property in base type 'I1'.
}