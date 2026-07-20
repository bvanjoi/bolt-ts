// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/promiseIdentityWithConstraints.ts`, Apache-2.0 License

//@compiler-options: target=es2015

export interface IPromise<T, V> {
    then<U extends T, W extends V>(callback: (x: T) => IPromise<U, W>): IPromise<U, W>;
}
export interface Promise<T, V> {
    then<U extends T, W extends V>(callback: (x: T) => Promise<U, W>): Promise<U, W>;
}

// Error because constraint V doesn't match
var x: IPromise<string, number>;
var x: Promise<string, boolean>;
//~^ ERROR: Subsequent variable declarations must have the same type. Variable 'x' must be of type 'IPromise<string, number>', but here has type 'Promise<string, boolean>'.