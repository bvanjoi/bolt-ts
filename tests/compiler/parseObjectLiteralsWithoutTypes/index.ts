// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/parseObjectLiteralsWithoutTypes.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

let x: { foo, bar }
let y: { foo: number, bar }
let z: { foo, bar: number }
