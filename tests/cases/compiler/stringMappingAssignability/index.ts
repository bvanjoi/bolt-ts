// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/stringMappingAssignability.ts`, Apache-2.0 License

const x: Uppercase<string> = 42;
//~^ ERROR: Type 'number' is not assignable to type 'Uppercase'.
const y: Uppercase<string> = { foo: "bar" };
//~^ ERROR: Type '{ foo: string; }' is not assignable to type 'Uppercase'.
