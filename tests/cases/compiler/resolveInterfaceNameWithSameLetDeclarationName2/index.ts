// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/resolveInterfaceNameWithSameLetDeclarationName2.ts`, Apache-2.0 License

interface foo { }
interface bar { }
let bar: bar | foo;
let foo: bar | foo;