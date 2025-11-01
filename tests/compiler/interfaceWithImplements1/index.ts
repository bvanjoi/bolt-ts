// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/interfaceWithImplements1.ts`, Apache-2.0 License

interface IFoo { }

interface IBar implements IFoo {
  //~^ ERROR: Interface declaration cannot have 'implements' clause.
}