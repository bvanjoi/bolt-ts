// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/MemberAccessorDeclaration15.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class C {
   set Foo(public a: number) { }  //~ERROR: A parameter property is only allowed in a constructor implementation.
}