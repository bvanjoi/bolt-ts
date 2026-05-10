// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/nestedSelf.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace M {
 export class C {
   public n = 42;
   public foo() { [1,2,3].map((x) => { return this.n * x; })}
 }
}