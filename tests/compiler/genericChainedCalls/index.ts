// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/genericChainedCalls.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface I1<T> {
    func<U>(callback: (value: T) => U): I1<T>;
}
 
declare var v1: I1<number>;
 
var r1 = v1.func(num => num.toString()) 
           .func(str => str.length) // error, number doesn't have a length
           //~^ ERROR: Property 'length' does not exist on type 'number'.
           .func(num => num.toString())
 
var s1 = v1.func(num => num.toString()) 
var s2 = s1.func(str => str.length) // should also error
           //~^ ERROR: Property 'length' does not exist on type 'number'.
var s3 = s2.func(num => num.toString())
