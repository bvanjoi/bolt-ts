// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/extendBaseClassBeforeItsDeclared.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class derived extends base { }
//~^ ERROR: Class 'base' used before its declaration.
//~| ERROR: Class 'base' used before its declaration.
 
class base { constructor (public n: number) { } }
