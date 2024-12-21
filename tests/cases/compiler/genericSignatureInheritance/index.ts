// From `github.com/microsoft/TypeScript/blob/v5.7.2/tests/cases/compiler/genericSignatureInheritance.ts`, Apache-2.0 License

interface I {
  <T>(x: T): string;
}

interface I2 extends I { }
