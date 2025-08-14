// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/specializationOfExportedClass.ts`, Apache-2.0 License

module M {

export class C<T> { }
  
}
   
var x = new M.C<string>();
  