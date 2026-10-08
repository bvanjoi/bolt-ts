// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/bestCommonTypeReturnStatement.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: allowUnreachableCode

interface IPromise<T> {
    then(successCallback: (promiseValue: T) => any, errorCallback?: (reason: any) => any): IPromise<any>;
}

function f() {
    if (true) return b();
    return d();
}


function b(): IPromise<void> { return null; }
//~^ ERROR: Type 'null' is not assignable to type 'IPromise<void>'.
function d(): IPromise<any> { return null; }
//~^ ERROR: Type 'null' is not assignable to type 'IPromise<any>'.