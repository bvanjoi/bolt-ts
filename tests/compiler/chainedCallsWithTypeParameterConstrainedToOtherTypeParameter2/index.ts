// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/chainedCallsWithTypeParameterConstrainedToOtherTypeParameter2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class Chain<T> {
    constructor(public value: T) { }
    then<S extends T>(cb: (x: T) => S): Chain<S> {
        var t!: T;
        var s!: S;
        // Ok to go down the chain, but error to climb up the chain
        (new Chain(t)).then(tt => s).then(ss => t);
        //~^ ERROR: Type 'T' is not assignable to type 'S'.

        // But error to try to climb up the chain
        (new Chain(s)).then(ss => t);
        //~^ ERROR: Type 'T' is not assignable to type 'S'.

        // Staying at T or S should be fine
        (new Chain(t)).then(tt => t).then(tt => t).then(tt => t);
        (new Chain(s)).then(ss => s).then(ss => s).then(ss => s);

        return null;
    }
}

// Similar to above, but T is now constrained. Verify that the constraint is maintained across invocations
interface I {
    x: number;
}
class Chain2<T extends I> {
    constructor(public value: T) { }
    then<S extends T>(cb: (x: T) => S): Chain2<S> {
        var i!: I;
        var t!: T;
        var s!: S;
        // Ok to go down the chain, check the constraint at the end.
        // Should get an error that we are assigning a string to a number
        (new Chain2(i)).then(ii => t).then(tt => s).value.x = "";
        //~^ ERROR: Type 'string' is not assignable to type 'number'.

        // Staying at T or S should keep the constraint.
        // Get an error when we assign a string to a number in both cases
        (new Chain2(i)).then(ii => t).then(tt => t).then(tt => t).then(tt => t).value.x = "";
        //~^ ERROR: Type 'string' is not assignable to type 'number'.
        (new Chain2(i)).then(ii => s).then(ss => s).then(ss => s).then(ss => s).value.x = "";
        //~^ ERROR: Type 'string' is not assignable to type 'number'.

        return null;
    }
}
