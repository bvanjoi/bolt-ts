// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/parameterReferencesOtherParameter2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class Model {
    public name: string;
    //~^ ERROR: Property 'name' has no initializer and is not definitely assigned in the constructor.
}

class UI {
    constructor(model: Model, foo = model.name)
    {
    }
}