// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/promiseTypeInference.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@run-fail

declare class CPromise<T> {
    then<U>(success?: (value: T) => CPromise<U>): CPromise<U>;
}
interface IPromise<T> {
    then<U>(success?: (value: T) => IPromise<U>): IPromise<U>;
}
declare function load(name: string): CPromise<string>;
declare function convert(s: string): IPromise<number>;

var $$x = load("something").then(s => convert(s));
