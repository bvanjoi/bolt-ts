// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/modularizeLibrary_ErrorFromUsingWellknownSymbolWithOutES6WellknownSymbolLib.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: lib=[es5,es2015.core]

function f(x: number, y: number, z: number) {
    return Array.from(arguments);
}

f(1, 2, 3);   // no error
let a = ['c', 'd'];
a[Symbol.isConcatSpreadable] = false;
//~^ ERROR: Type 'boolean' is not assignable to type 'string'.
//~| ERROR: Cannot find name 'Symbol'.
