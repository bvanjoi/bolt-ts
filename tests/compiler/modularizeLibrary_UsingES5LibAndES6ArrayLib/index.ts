// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/modularizeLibrary_UsingES5LibAndES6ArrayLib.ts`, Apache-2.0 License

//@compiler-options: lib=[es5, es2015.core]
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

// No error
function f(x: number, y: number, z: number) {
    return Array.from(arguments);
}

f(1, 2, 3);