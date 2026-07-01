// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/promiseIdentity2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
export interface IPromise<T, V> {
    then<U, W>(callback: (x: T) => IPromise<U, W>): IPromise<U, W>;
}
export interface Promise<T, V> {
    then<U, W>(callback: (x: T) => Promise<T, U>): Promise<T, W>;
}

// error because T is string in the first declaration, and T is boolean in the second
// Return type and callback return type are ok because T is any in this particular Promise
var x: IPromise<string, number>;
var x: Promise<any, string>;
//~^ ERROR: Subsequent variable declarations must have the same type. Variable 'x' must be of type 'IPromise<string, number>', but here has type 'Promise<any, string>'.