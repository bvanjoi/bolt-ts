// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/genericAndNonGenericOverload1.ts`, Apache-2.0 License

interface callable2<T> {
  (a: T): T;
  <Z>(a: T): Z;
}
var c2: callable2<number>;
var a: number = c2<string>(1);
//~^ ERROR: Type 'string' is not assignable to type 'number'.