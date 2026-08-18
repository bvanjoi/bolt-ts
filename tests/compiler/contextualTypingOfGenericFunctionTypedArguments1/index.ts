// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contextualTypingOfGenericFunctionTypedArguments1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
interface Collection<T> {
    length: number;
    add(x: T): void;
    remove(x: T): boolean;
}

interface Combinators {
    forEach<T>(c: Collection<T>, f: (x: T) => Date): void;
}

declare var c2: Collection<number>;
declare var _: Combinators;

// errors on all 3 lines, bug was that r5 was the only line with errors
var f = (x: number) => { return x.toFixed() };
var r5 = _.forEach<number>(c2, f); 
//~^ ERROR: Argument of type '(x: number) => string' is not assignable to parameter of type '(x: number) => Date'.
var r6 = _.forEach<number>(c2, (x) => { return x.toFixed() }); 
//~^ ERROR: Argument of type '(x: number) => string' is not assignable to parameter of type '(x: number) => Date'.
