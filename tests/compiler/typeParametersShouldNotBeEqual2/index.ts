// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/typeParametersShouldNotBeEqual2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function ff<T extends Date, U extends Date, V>(x: T, y: U, z: V) {
    var zz!: Object;
    x = x;  // Ok
    x = y;  // Ok
    //~^ ERROR: Type 'U' is not assignable to type 'T'.
    x = z;  // Error
    //~^ ERROR: Type 'V' is not assignable to type 'T'.
    z = x;  // Error
    //~^ ERROR: Type 'T' is not assignable to type 'V'.
    y = z;  // Error
    //~^ ERROR: Type 'V' is not assignable to type 'U'.
    z = y;  // Error
    //~^ ERROR: Type 'U' is not assignable to type 'V'.
    x = zz;  // Error
    //~^ ERROR: Type 'Object' is not assignable to type 'T'.
    zz = x;  // Ok
}
