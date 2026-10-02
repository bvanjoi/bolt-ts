// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericCallWithinOwnBodyCastTypeParameterIdentity.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

interface Thenable<Value> {
    then<V>(
        onFulfilled: (value: Value) => V | Thenable<V>,
    ): Thenable<V>;
}

const toThenable = <Result, Input>(fn: (input: Input) => Result | Thenable<Result>) =>
    (input: Input): Thenable<Result> => {
        const result = fn(input)
        return {
            then<V>(onFulfilled: (value: Result) => V | Thenable<V>) {
                return toThenable<V, Result>(onFulfilled)(result as Result)
            }
        };
    }

const toThenableInferred = <Result, Input>(fn: (input: Input) => Result | Thenable<Result>) =>
    (input: Input): Thenable<Result> => {
        const result = fn(input)
        return {
            then(onFulfilled) {
                return toThenableInferred(onFulfilled)(result as Result)
            }
        };
    }

interface I {
    f1<V>(f: () => V): void;
}

const i: I = {
    f1<V>(f: () => V) {}
};