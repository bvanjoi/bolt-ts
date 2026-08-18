// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/ipromise4.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare namespace Windows.Foundation {
    export interface IPromise<T> {
        then<U>(success?: (value: T) => IPromise<U>, error?: (error: any) => IPromise<U>, progress?: (progress: any) => void ): Windows.Foundation.IPromise<U>;
        then<U>(success?: (value: T) => IPromise<U>, error?: (error: any) => U, progress?: (progress: any) => void ): Windows.Foundation.IPromise<U>;
        then<U>(success?: (value: T) => U, error?: (error: any) => IPromise<U>, progress?: (progress: any) => void ): Windows.Foundation.IPromise<U>;
        then<U>(success?: (value: T) => U, error?: (error: any) => U, progress?: (progress: any) => void ): Windows.Foundation.IPromise<U>;
        done? <U>(success?: (value: T) => any, error?: (error: any) => any, progress?: (progress: any) => void ): void;
    }
}
 
var p: Windows.Foundation.IPromise<number> = null;
//~^ ERROR: Type 'null' is not assignable to type 'Windows.Foundation.IPromise<number>'.
 
p.then(function (x) { } ); // should not error
p.then(function (x) { return "hello"; } ).then(function (x) { return x } ); // should not error
 
