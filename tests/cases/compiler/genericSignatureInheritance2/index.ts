// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/genericSignatureInheritance2.ts`, Apache-2.0 License

interface I {
  <T>(x: T): string;
}

interface I2 extends I { 
  <T>(x: T): void;
}
