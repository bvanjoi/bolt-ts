// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/functionOverloadsOnGenericArity2.ts`, Apache-2.0 License

interface I {
  then(p: string): string;
  then<U>(p: string): string;
  then<U, T>(p: string): Date;
}