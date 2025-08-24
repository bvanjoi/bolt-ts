// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/crashOnMethodSignatures.ts`, Apache-2.0 License

class A {
  a(completed: () => any): void;
  //~^ ERROR: Function implementation is missing or not immediately following the declaration.
}
