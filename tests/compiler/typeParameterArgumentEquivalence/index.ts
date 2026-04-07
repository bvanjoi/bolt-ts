// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/typeParameterArgumentEquivalence.ts`, Apache-2.0 License

//@compiler-options: target=es2015
function foo<T>() {
    var x!: (item: number) => boolean;
    var y!: (item: T) => boolean;
    x = y;  // Should be an error
    //~^ ERROR: Type '(item: T) => boolean' is not assignable to type '(item: number) => boolean'.
    y = x;  // Shound be an error
    //~^ ERROR: Type '(item: number) => boolean' is not assignable to type '(item: T) => boolean'.
}
