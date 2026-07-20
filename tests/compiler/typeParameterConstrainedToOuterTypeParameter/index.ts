// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeParameterConstrainedToOuterTypeParameter.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

interface A<T> {
  <U extends T>(x: U[])
}

interface B<T> {
  <U extends T>(x: U)
}

var a: A<string>
var b: B<string> = a; // assignment should be legal (both U's get instantiated to any for comparison)
//~^ ERROR: Type 'A<string>' is not assignable to type 'B<string>'.


interface C<T> {
  <U extends T>(x: U[])
}

var c: C<string>
var d: C<string> = c;
a = c;
