// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/bind1.ts`, Apache-2.0 License

module M {
  export class C implements I {} // this should be an unresolved symbol I error
  //~^ ERROR: Cannot find name 'I'.
}

