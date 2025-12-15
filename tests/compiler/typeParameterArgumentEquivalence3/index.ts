// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/typeParameterArgumentEquivalence3.ts`, Apache-2.0 License

function foo<T,U>() {
    var x: (item) => T;
    var y: (item) => boolean;
    x = y;  // Should be an error
    //~^ ERROR: Type '(item: any) => boolean' is not assignable to type '(item: any) => T'.
    y = x;  // Shound be an error
    //~^ ERROR: Type '(item: any) => T' is not assignable to type '(item: any) => boolean'.
}
