// From `github.com/microsoft/TypeScript/blob/6.0.3/tests/cases/compiler/typeParameterConstrainedToOuterTypeParameter2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

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
