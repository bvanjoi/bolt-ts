// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/typeGuardNarrowsIndexedAccessOfKnownProperty6.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

declare const aIndex: "key";
const a: { key?: { x?: number } } = {};
if (a[aIndex] && a[aIndex].x) {
    a[aIndex].x // number
    const d: string = a[aIndex].x;
    //~^ ERROR: Type 'number' is not assignable to type 'string'.
}

declare const bIndex: "key";
const b: { key: { x?: number } } = { key: {} };
if (b[bIndex].x) {
    b[bIndex].x // number
    const d: string = b[bIndex].x;
    //~^ ERROR: Type 'number' is not assignable to type 'string'.
}

declare const cIndex: 1;
interface Foo {
    x: number | undefined;
}
const c: Foo[] = [];
if (c[cIndex].x) {
    c[cIndex].x // number
    const d: string = c[cIndex].x;
    //~^ ERROR: Type 'number' is not assignable to type 'string'.
}
