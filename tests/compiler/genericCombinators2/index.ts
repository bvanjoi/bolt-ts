// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericCombinators2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface Collection<T, U> {
    length: number;
    add(x: T, y: U): void;
    remove(x: T, y: U): boolean;
}

interface Combinators {
    map<T, U>(c: Collection<T, U>, f: (x: T, y: U) => any): Collection<any, any>;
    map<T, U, V>(c: Collection<T, U>, f: (x: T, y: U) => V): Collection<T, V>;
}

declare var _: Combinators;
declare var c2: Collection<number, string>;
var rf1 = (x: number, y: string) => { return x.toFixed() };
var r5a = _.map<number, string, Date>(c2, (x, y) => { return x.toFixed() });
//~^ ERROR: No overload matches this call.
var r5b = _.map<number, string, Date>(c2, rf1);
//~^ ERROR: No overload matches this call.
var r5c = _.map<number, string, Date>(c2, (_x, _y) => new Date);
