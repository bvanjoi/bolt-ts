// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/unqualifiedCallToClassStatic1.ts`, Apache-2.0 License

class Vector {
 static foo = () => {
  // 'foo' cannot be called in an unqualified manner.
  foo();
  //~^ ERROR: Cannot find name 'foo'.
 }
}