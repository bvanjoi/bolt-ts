// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/objectLiteralsAgainstUnionsOfArrays01.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface Foo {
  bar: Bar | Bar[];
}

interface Bar {
  prop: string;
}

let x: Foo[] = [
  { bar: { prop: 100 } }
  //~^ ERROR: Type '{ prop: number; }' is not assignable to type 'Bar | Bar[]'.
]

let y: Foo[] = [
  { bar: [{ prop: 100 }] }
  //~^ ERROR: Type 'number' is not assignable to type 'string'.
]