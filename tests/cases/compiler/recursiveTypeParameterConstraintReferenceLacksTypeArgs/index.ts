// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/recursiveTypeParameterConstraintReferenceLacksTypeArgs.ts`, Apache-2.0 License

class A<T extends A> { }
//~^ ERROR: Generic type 'A<T>' requires 1 type argument.