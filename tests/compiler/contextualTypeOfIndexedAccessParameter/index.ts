// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/contextualTypeOfIndexedAccessParameter.ts`, Apache-2.0 License

//@compiler-options: strict
//@run-fail

type Keys = "a" | "b";

type OptionsForKey = { a: { cb: (p: number) => number } } & { b: {} };

declare function f<K extends Keys>(key: K, options: OptionsForKey[K]): void;

f("a", {
    cb: p => p,
});

function g<
    K extends "a" | "b">(x: ({ a: string } & { b: string })[K], y: string) {
    x = y;
}
