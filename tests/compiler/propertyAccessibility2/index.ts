// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/propertyAccessibility2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class C {
  private static x = 1;
}
var c = C.x;
//~^ ERROR: Property 'x' is private and only accessible within class 'C'.