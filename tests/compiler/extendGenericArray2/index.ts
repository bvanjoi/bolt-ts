// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/extendGenericArray2.ts`, Apache-2.0 License

interface IFoo<T> {
    x: T;
}

interface Array<T> extends IFoo<T> { }

var arr: string[] = [];
var y: number = arr.x;
//~^ ERROR: Type 'string' is not assignable to type 'number'.