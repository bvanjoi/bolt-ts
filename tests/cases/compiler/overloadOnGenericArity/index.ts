// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/overloadOnConstInheritance3.ts`, Apache-2.0 License

interface Test {
  // then<U>(p: string): string;
  then(p: string): Date; // Error: Overloads cannot differ only by return type
}

