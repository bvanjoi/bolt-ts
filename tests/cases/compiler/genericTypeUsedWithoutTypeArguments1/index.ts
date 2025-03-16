// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/genericTypeUsedWithoutTypeArguments1.ts`, Apache-2.0 License

interface Foo<T> { }
class Bar<T> implements Foo { }
//~^ ERROR: Generic type 'Foo<T>' requires 1 type argument.