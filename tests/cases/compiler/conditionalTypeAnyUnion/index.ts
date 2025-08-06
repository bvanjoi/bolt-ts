// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/conditionalTypeAnyUnion.ts`, Apache-2.0 License

type Spec = any extends object ? any : string;

type WithSpec<T extends number> = T

type R = WithSpec<Spec> // should not error