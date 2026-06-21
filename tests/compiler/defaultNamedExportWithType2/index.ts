// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/defaultNamedExportWithType2.ts`, Apache-2.0 License

//@compiler-options: target=esnext

type Foo = number;
const Foo = 1;
export default Foo;
