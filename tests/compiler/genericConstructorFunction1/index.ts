// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericConstructorFunction1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function f1<T>(args: T) {
    var v1!: { [index: string]: new (arg: T) => Date };
    var v2 = v1['test'];
    v2(args); 
    //~^ ERROR: Value of type 'new (arg: T) => Date' is not callable.
    return new v2(args); // used to give error
}


interface I1<T> { new (arg: T): Date };
function f2<T>(args: T) {
    var v1!: { [index: string]: I1<T> };
    var v2 = v1['test'];
    var y = v2(args); 
    //~^ ERROR: Value of type 'I1<T>' is not callable.
    return new v2(args); // used to give error
}