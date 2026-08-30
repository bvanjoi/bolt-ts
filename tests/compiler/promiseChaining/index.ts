// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/promiseChaining.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class Chain<T> {
    constructor(public value: T) { }
    then<S>(cb: (x: T) => S): Chain<S> {
        var result = cb(this.value);
        // should get a fresh type parameter which each then call
        var z = this.then(x => result)/*S*/.then(x => "abc")/*string*/.then(x => x.length)/*number*/; // No error
        return new Chain(result);
    }
}

