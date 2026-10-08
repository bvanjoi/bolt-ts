// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeParameterExtendsPrimitive.ts`, Apache-2.0 License

//@compiler-options: target=es2015

// #14473
function f<T extends number>() {
    var t: T;
    var v = {
        [t]: 0
        //~^ ERROR: Variable 't' is used before being assigned.
    }
    return t + t;
    //~^ ERROR: Variable 't' is used before being assigned.
    //~| ERROR: Variable 't' is used before being assigned.
}

// #15501
interface I { x: number }
type IdMap<T> = { [P in keyof T]: T[P] };
function g<T extends I>(i: IdMap<T>) {
    const n: number = i.x;
    return i.x * 2;
}

// #17069
function h<T extends Record<K, number>, K extends string>(array: T[], prop: K): number {
    let result = 0;
    for (const v of array) {
        result += v[prop];
    }
    return result;
}
