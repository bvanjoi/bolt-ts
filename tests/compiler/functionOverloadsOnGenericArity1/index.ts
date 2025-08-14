// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/functionOverloadsOnGenericArity1.ts`, Apache-2.0 License

interface C {
  f<T>(): string;
  f<T, U>(): string; 

  <T>(): string;
  <T, U>(): string; 

 new <T>(): string;
 new <T, U>(): string; 
}
