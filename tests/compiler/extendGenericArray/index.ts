// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/extendGenericArray.ts`, Apache-2.0 License

interface Array<T> {
    foo(): T;
}

var arr: string[] = [];
var x: number = arr.foo();
//~^ ERROR: Type 'string' is not assignable to type 'number'.