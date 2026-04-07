// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/contextuallyTypingRestParameters.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var x: (...y: string[]) => void = function (.../*3*/y) { 
    var t = y; 
    var x2: string = t; // This should be error
    //~^ ERROR: Type 'string[]' is not assignable to type 'string'.
    var x3: string[] = t; // No error
    var y2: string = y; // This should be error
    //~^ ERROR: Type 'string[]' is not assignable to type 'string'.
    var y3: string[] = y; // No error
};