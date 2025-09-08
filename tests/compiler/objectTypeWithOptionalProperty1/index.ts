// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/objectTypeWithOptionalProperty1.ts`, Apache-2.0 License

    var b = {
        x?: 1 // error
        //~^ ERROR: An object member cannot be declared optional.
    }