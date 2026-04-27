// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/templateLiteralIntersection.ts`, Apache-2.0 License

//@compiler-options: target=es2015

const a = 'a'

type A = typeof a
type MixA = A & {foo: string}

type OriginA1 = `${A}`
type OriginA2 = `${MixA}`

type B = `${typeof a}`
type MixB = B & { foo: string }

type OriginB1 = `${B}`
type OriginB2 = `${MixB}`

type MixC = { foo: string } & A

type OriginC = `${MixC}`

type MixD<T extends string> =
    `${T & { foo: string }}`
type OriginD = `${MixD<A & { foo: string }> & { foo: string }}`;

type E = `${A & {}}`;
type MixE = E & {}
type OriginE = `${MixE}`

type OriginF = `${A}foo${A}`;

function f() {
    const b = {
        c: 1
    };
    const d = a ? `\${${b.c++}}` : "";
    let type = 2;
    type = 1;
}