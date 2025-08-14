// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/genericTypeUsedWithoutTypeArguments3.ts`, Apache-2.0 License

interface Foo<T> { }
interface Bar<T> extends Foo { }
//~^ ERROR: Generic type 'Foo<T>' requires 1 type argument.