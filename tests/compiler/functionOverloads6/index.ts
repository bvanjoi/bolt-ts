// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/functionOverloads6.ts`, Apache-2.0 License

class foo { 
  static fnOverload(); //~ ERROR: 'fnOverload', which lacks return-type annotation, implicitly has an 'any' return type.
  static fnOverload(foo:string); //~ ERROR: 'fnOverload', which lacks return-type annotation, implicitly has an 'any' return type.
  static fnOverload(foo?: any){ }
}
