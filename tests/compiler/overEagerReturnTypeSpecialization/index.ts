// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/overEagerReturnTypeSpecialization.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface I1<T> {
    func<U>(callback: (value: T) => U): I1<U>;
}
 
declare var v1: I1<number>;
var r1: I1<string> = v1.func(num => num.toString()) // Correctly returns an I1<string>
           .func(str => str.length);    // should error
//~^^ ERROR: Type 'I1<number>' is not assignable to type 'I1<string>'.
var r2: I1<number> = v1.func(num => num.toString()) // Correctly returns an I1<string>
           .func(str => str.length);    // should be ok 
 
