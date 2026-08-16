// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/errorInUnnamedClassExpression.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

// https://github.com/microsoft/TypeScript/issues/62920

let Foo = class {
    constructor() {
        this.bar++;
        //~^ ERROR: Abstract property 'bar' in class '(Anonymous class)' cannot be accessed in the constructor.
    }
    abstract bar;
    //~^ ERROR: Abstract modifier can only appear within an abstract class.
    //~| ERROR: Member 'bar' implicitly has an 'any' type.
};
