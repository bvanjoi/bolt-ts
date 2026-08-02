// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/promiseChaining2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

// same example but with constraints on each type parameter
class Chain2<T extends { length: number }> {
    constructor(public value: T) { }
    then<S extends Function>(cb: (x: T) => S): Chain2<S> {
        var result = cb(this.value);
        // should get a fresh type parameter which each then call
        var z = this.then(x => result).then(x => "abc").then(x => x.length);
        //~^ ERROR: Type 'string' is not assignable to type 'Function'.
        //~| ERROR: Type 'number' is not assignable to type 'Function'.
        return new Chain2(result);
    }
}