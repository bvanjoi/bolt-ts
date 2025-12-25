// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/contextualTypeObjectSpreadExpression.ts`, Apache-2.0 License

interface I {
    a: "a";
}
let i: I;
i = { ...{ a: "a" } };
i = { a: 'a' };


let j: { a: string }
j = { ...{ a: "a" } };
j = { a: 'a' };