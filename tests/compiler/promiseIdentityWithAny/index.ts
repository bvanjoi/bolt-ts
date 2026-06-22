// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/promiseIdentityWithAny.ts`, Apache-2.0 License

//@compiler-options: target=es2015
export interface IPromise<T, V> {
    then<U, W>(callback: (x: T) => IPromise<U, W>): IPromise<U, W>;
}
export interface Promise<T, V> {
    then<U, W>(callback: (x: T) => Promise<any, any>): Promise<any, any>;
}

// Should be ok because signature type parameters get erased to any
var x: IPromise<string, number>;
var x: Promise<string, boolean>;
//~^ ERROR: Subsequent variable declarations must have the same type. Variable 'x' must be of type 'IPromise<string, number>', but here has type 'Promise<string, boolean>'.