// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/recursiveInheritanceGeneric.ts`, Apache-2.0 License

interface I5<T> extends I5<T> { 
  //~^ ERROR: Type 'I5<T>' recursively references itself as a base type.
  foo():void;
}  