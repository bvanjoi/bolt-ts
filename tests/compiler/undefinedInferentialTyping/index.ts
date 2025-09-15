// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/undefinedInferentialTyping.ts`, Apache-2.0 License

function f<T>(arr: T[], elemnt: T): T {
    return null;
}

var a = f([], 3); // should be number
var a1: string = a;
//~^ ERROR: Type 'number' is not assignable to type 'string'.
var b: number = f([], 3);
var c: string = f([], '');