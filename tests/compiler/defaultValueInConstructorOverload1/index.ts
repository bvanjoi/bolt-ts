// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/defaultValueInConstructorOverload1.ts`, Apache-2.0 License

class C {
    constructor(x = '');
    //~^ ERROR: A parameter initializer is only allowed in a function or constructor implementation.
    constructor(x = '') {
    }
}