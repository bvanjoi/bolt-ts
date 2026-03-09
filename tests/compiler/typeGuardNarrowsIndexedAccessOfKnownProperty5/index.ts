// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/typeGuardNarrowsIndexedAccessOfKnownProperty5.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

const a: { key?: { x?: number } } = {};
const aIndex = "key";
if (a[aIndex] && a[aIndex].x) {
    a[aIndex].x // number
}

const b: { key: { x?: number } } = { key: {} };
const bIndex = "key";
if (b[bIndex].x) {
    b[bIndex].x // number
    const d: string = b[bIndex].x;
    //~^ ERROR: Type 'number' is not assignable to type 'string'.
}

interface Foo {
    x: number | undefined;
}
const c: Foo[] = [];
const cIndex = 1;
if (c[cIndex].x) {
    c[cIndex].x // number
    const d: string = c[cIndex].x;
    //~^ ERROR: Type 'number' is not assignable to type 'string'.
}
