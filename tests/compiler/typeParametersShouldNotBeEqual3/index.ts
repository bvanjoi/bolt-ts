// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/typeParametersShouldNotBeEqual2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function ff<T extends Object, U extends Object>(x: T, y: U) {
    var z!: Object;
    x = x;  // Ok
    x = y;  // Ok
    //~^ ERROR: Type 'U' is not assignable to type 'T'.
    x = z;  // Ok
    //~^ ERROR: Type 'Object' is not assignable to type 'T'.
    z = x;  // Ok
}
