// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/unknownPropertiesAreAssignableToObjectUnion.ts`, Apache-2.0 License

//@compiler-options: strictNullChecks

const x: Object | string = { x: 0 };
const y: Object | undefined = { x: 0 };
