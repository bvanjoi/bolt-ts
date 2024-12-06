// From `github.com/microsoft/TypeScript/blob/v5.7.2/tests/cases/compiler/genericNumberIndex.ts`, Apache-2.0 License

type X<I extends number> = ['a'][I];
