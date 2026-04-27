// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/extendGlobalThis2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

namespace globalThis {  //~ERROR: Declaration name conflicts with built-in global identifier 'globalThis'.
    export function foo() { console.log("x"); }
}