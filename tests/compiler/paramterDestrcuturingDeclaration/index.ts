// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/paramterDestrcuturingDeclaration.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: declaration

interface C {
    ({p: name}): any;
    //~^ ERROR: 'name' is an unused renaming of 'p'. Did you intend to use it as a type annotation?
    new ({p: boolean}): any;
    //~^ ERROR: 'boolean' is an unused renaming of 'p'. Did you intend to use it as a type annotation?
}

interface D {
  (a: {p: string}): any;
}

