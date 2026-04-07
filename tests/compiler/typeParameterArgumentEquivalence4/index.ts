// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/typeParameterArgumentEquivalence4.ts`, Apache-2.0 License

//@compiler-options: target=es2015
function foo<T,U>() {
    var x!: (item: any) => U;
    var y!: (item: any) => T;
    x = y;  // Should be an error
    //~^ ERROR: Type '(item: any) => T' is not assignable to type '(item: any) => U'.
    y = x;  // Shound be an error
    //~^ ERROR: Type '(item: any) => U' is not assignable to type '(item: any) => T'.
}
