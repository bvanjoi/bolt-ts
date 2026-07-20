// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeParameterArgumentEquivalence3.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function foo<T,U>() {
    var x!: (item: any) => T;
    var y!: (item: any) => boolean;
    x = y;  // Should be an error
    //~^ ERROR: Type '(item: any) => boolean' is not assignable to type '(item: any) => T'.
    y = x;  // Shound be an error
    //~^ ERROR: Type '(item: any) => T' is not assignable to type '(item: any) => boolean'.
}
