// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/errorOnInitializerInObjectTypeLiteralProperty.ts`, Apache-2.0 License

var Foo: {
    bar: number = 5;
    //~^ ERROR: A type literal property cannot have an initializer.
};

let Bar: {
    bar: number = 5;
    //~^ ERROR: A type literal property cannot have an initializer.
};

