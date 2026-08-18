// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/optionalFunctionArgAssignability.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface Promise<T> {
    then<U>(onFulfill?: (value: T) => U, onReject?: (reason: any) => U): Promise<U>;
}
 
var a = function then<U>(onFulfill?: (value: string) => U, onReject?: (reason: any) => U): Promise<U> { return null };
//~^ ERROR: Type 'null' is not assignable to type 'Promise<U>'.
var b = function then<U>(onFulFill?: (value: number) => U, onReject?: (reason: any) => U): Promise<U> { return null };
//~^ ERROR: Type 'null' is not assignable to type 'Promise<U>'.
a = b; // error because number is not assignable to string
//~^ ERROR: Type '<U>(onFulFill: undefined | ((value: number) => U), onReject: undefined | ((reason: any) => U)) => Promise<U>' is not assignable to type '<U>(onFulfill: undefined | ((value: string) => U), onReject: undefined | ((reason: any) => U)) => Promise<U>'.
