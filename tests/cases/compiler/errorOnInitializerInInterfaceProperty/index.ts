// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/errorOnInitializerInInterfaceProperty.ts`, Apache-2.0 License

interface Foo {
    bar: number = 5;
    //~^ ERROR:  An interface property cannot have an initializer.
}
