// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/incompatibleGenericTypes.ts`, Apache-2.0 License

interface I1<T> {

  m1<U>(callback: (p: T) => U): I1<U>;

}
 
var v1: I1<boolean>;
 
var v2: I1<number> = v1;
//~^ ERROR: Type 'I1<boolean>' is not assignable to type 'I1<number>'.