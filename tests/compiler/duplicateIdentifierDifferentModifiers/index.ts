// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/duplicateIdentifierDifferentModifiers.ts`, Apache-2.0 License

// Not OK
interface B { x; }
//~^ ERROR: All declarations of 'x' must have identical modifiers.
interface B { x?; }

// OK
class A {
  public y: string;
}

interface A {
  y: string;
}

// Not OK
class C {
  private y: string;
//~^ ERROR: All declarations of 'y' must have identical modifiers.
}

interface C {
  y: string;
}
