// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/indexedAccessCanBeHighOrder.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@run-fail

declare function get<U, Y extends keyof U>(x: U, y: Y): U[Y];
declare function find<T, K extends keyof T>(o: T[K]): [T, K];

function impl<A, B extends keyof A>(a: A, b: B) {
    const item = get(a, b);
    return find(item);
}

const o = {x: 42};
const r = impl(o, "x");
r[0][r[1]] = o[r[1]];