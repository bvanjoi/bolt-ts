// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/defaultNamedExportWithType1.ts`, Apache-2.0 License

//@compiler-options: target=esnext

type Foo = number;
export const Foo = 1;
export default Foo;
