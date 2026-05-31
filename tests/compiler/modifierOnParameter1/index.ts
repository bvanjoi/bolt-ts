// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/modifierOnParameter1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class C {
   constructor(declare p) { }
   //~^ ERROR: 'declare' modifier cannot appear on a parameter.
}