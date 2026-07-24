// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericFunctionsWithOptionalParameters3.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class Collection<T> {
    public add(x: T) { }
}
interface Utils {
    fold<T, S>(c?: Collection<T>, folder?: (s: S, t: T) => T, init?: S): T;
    mapReduce<T, U, V>(c: Collection<T>, mapper: (x: T) => U, reducer: (y: U) => V): Collection<V>;
}
var utils: Utils;
var c = new Collection<string>();
var r3 = utils.mapReduce(c, (x) => { return 1 }, (y) => { return new Date() });
//~^ ERROR: Variable 'utils' is used before being assigned.
//~| ERROR: Variable 'utils' is used before being assigned.
var r4 = utils.mapReduce(c, (x: string) => { return 1 }, (y: number) => { return new Date() });
//~^ ERROR: Variable 'utils' is used before being assigned.
//~| ERROR: Variable 'utils' is used before being assigned.
var f1 = (x: string) => { return 1 };
var f2 = (y: number) => { return new Date() };
var r5 = utils.mapReduce(c, f1, f2);
//~^ ERROR: Variable 'utils' is used before being assigned.
//~| ERROR: Variable 'utils' is used before being assigned.