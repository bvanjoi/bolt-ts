// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/typeParameterConstrainedToOuterTypeParameter2.ts`, Apache-2.0 License

interface A<T> {
  foo<U extends T>(x: A<A<U>>): void 
}

interface B<T> {
  foo<U extends T>(x: B<B<U>>): void 
}

var a: A<string>
var b: B<string> = a;


a.foo(b)

function f3(a3: string) {
  a3 = a
  //~^ ERROR: Type 'A<string>' is not assignable to type 'string'.
}
