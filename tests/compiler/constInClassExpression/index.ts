// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/constInClassExpression.ts`, Apache-2.0 License

//@compiler-options: target=es2015

let C = class {
    const a = 4;
    //~^ ERROR: A class member cannot have the 'const' keyword.
};