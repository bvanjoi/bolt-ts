// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/genericIndexTypeHasSensibleErrorMessage.ts`, Apache-2.0 License

type Wat<T extends string> = { [x: T]: string };
//~^ ERROR: An index signature parameter type cannot be a literal type or generic type. Consider using a mapped object type instead.