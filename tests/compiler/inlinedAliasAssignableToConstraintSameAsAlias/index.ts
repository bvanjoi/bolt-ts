// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inlinedAliasAssignableToConstraintSameAsAlias.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface RelationFields {
  x: A;
  y: A[];
  z: A[];
}
type Name = keyof RelationFields;
type ShouldA<RF extends RelationFields, N extends Name> = RF[N] extends A[]
  ? RF[N][0]
  : never;

class A {
  x: A;
  //~^ ERROR: Property 'x' has no initializer and is not definitely assigned in the constructor.
  y: A[];
  //~^ ERROR: Property 'y' has no initializer and is not definitely assigned in the constructor.
  z: A[];
  //~^ ERROR: Property 'z' has no initializer and is not definitely assigned in the constructor.

  whereRelated< // Works // Type is same as A1, but is not assignable to type A
    RF extends RelationFields = RelationFields,
    N extends Name = Name,
    A1 extends A = RF[N] extends A[] ? RF[N][0] : never,
    A2 extends A = ShouldA<RF, N>
  >(): number {
    return 1;
  }
}