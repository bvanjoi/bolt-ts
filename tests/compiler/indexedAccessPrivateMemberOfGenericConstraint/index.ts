// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/indexedAccessPrivateMemberOfGenericConstraint.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class A {
  private a: number;
  //~^ ERROR: Property 'a' has no initializer and is not definitely assigned in the constructor.
}

class B {
  private a: string;
  //~^ ERROR: Property 'a' has no initializer and is not definitely assigned in the constructor.
}

type X<T extends A> = [T["a"], (T | B)["a"]];
//~^ ERROR: Private or protected member 'a' cannot be accessed on a type parameter.
//~| ERROR: Private or protected member 'a' cannot be accessed on a type parameter.
type Y<T extends A | B> = T["a"];
//~^ ERROR: Private or protected member 'a' cannot be accessed on a type parameter.
type Z<T extends A & B> = T["a"];
