// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/indexSignatureWithInitializer1.ts`, Apache-2.0 License

class C {
  [a: number = 1]: number;
  //~^ ERROR: An index signature parameter cannot have an initializer.
}