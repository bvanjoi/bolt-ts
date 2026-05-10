// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/lambdaParameterWithTupleArgsHasCorrectAssignability.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

type MyTupleItem = {};
type MyTuple = [MyTupleItem, ...MyTupleItem[]];

type GenericFunction<T extends MyTuple> = (...fromArgs: T) => void;

class GenericClass<T extends MyTuple> {
    from: GenericFunction<T> | undefined;
}

function createClass<T extends MyTuple>(f: GenericFunction<T>): GenericClass<T> {
    return new GenericClass<T>(/* ... use f */);
}

function consumeClass(c: GenericClass<[string, boolean]>) { }

// should work
consumeClass(createClass(str => console.log(str.length)));

// should work
consumeClass(createClass((str, _unused_num) => console.log(str.length)));


consumeClass(createClass((a, b) => {
    const c0: 42 = a;
    //~^ ERROR: Type 'string' is not assignable to type '42'.
    const c1: 42 = b;
    //~^ ERROR: Type 'boolean' is not assignable to type '42'.
}));

interface G<T extends [...{}[]]> {
    from: (...fromArgs: T) => void;
}

function f(a: G<[string]>, b: G<[string, number]>) {
    b = a; // should work
    a = b;
    //~^ ERROR: Type 'G<[string, number]>' is not assignable to type 'G<[string]>'.
}