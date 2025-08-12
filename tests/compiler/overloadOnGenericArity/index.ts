// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/overloadOnGenericArity.ts`, Apache-2.0 License

interface Test {
  then<U>(p: string): string;
  then(p: string): Date; // Error: Overloads cannot differ only by return type
}

