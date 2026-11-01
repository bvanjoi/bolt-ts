// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/circularInlineMappedGenericTupleTypeNoCrash.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

class Foo<Elements extends readonly unknown[]> {
  public readonly elements: { [P in keyof Elements]: { bar: Elements[P] } };

  public constructor(
    ...elements: { [P in keyof Elements]: { bar: Elements[P] } }
  ) {
    this.elements = elements;
  }

  public add(): Foo<[...Elements, "abc"]> {
    return new Foo<[...Elements, "abc"]>(...this.elements, { bar: "abc" });
    //~^ ERROR: Type instantiation is excessively deep and possibly infinite.
    //~| ERROR: Type instantiation is excessively deep and possibly infinite.
  }
}