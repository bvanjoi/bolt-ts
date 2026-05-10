// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typePredicatesCanNarrowByDiscriminant.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@run-fail

declare const fruit: { kind: 'apple'} | { kind: 'banana' } | { kind: 'cherry' }

declare function isOneOf<T, U extends T>(item: T, array: readonly U[]): item is U
if (isOneOf(fruit.kind, ['apple', 'banana'] as const)) {
    fruit.kind
    fruit
}

declare const fruit2: { kind: 'apple'} | { kind: 'banana' } | { kind: 'cherry' }
const kind = fruit2.kind;
if (isOneOf(kind, ['apple', 'banana'] as const)) {
    fruit2.kind
    fruit2
}