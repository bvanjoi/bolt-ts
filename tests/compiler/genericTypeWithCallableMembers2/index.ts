// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/genericTypeWithCallableMembers2.ts`, Apache-2.0 License

function foo1<T extends { (): string; }>(f: T) {
  return f(); // should return 'string', once returned 'any'
}

function foo2<T extends { new (): string; }>(f: T) {
  return new f(); // should be legal, once was an error
}

let a: string = foo1(() => 'hello');;