// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/functionOverloads6.ts`, Apache-2.0 License

class foo { 
  static fnOverload();
  static fnOverload(foo:string);
  static fnOverload(foo?: any){ }
}
