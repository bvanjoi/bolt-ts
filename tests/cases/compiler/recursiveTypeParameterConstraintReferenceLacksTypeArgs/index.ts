// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/recursiveTypeParameterConstraintReferenceLacksTypeArgs.ts`, Apache-2.0 License

class A<T extends A> { }
//~^ ERROR: Generic type 'A' requires 1 type argument.