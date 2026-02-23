// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/neverNullishThroughParentheses.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

// Repro for issue where "never nullish" checks miss "never nullish" through parentheses

const x: { y: string | undefined } | undefined = undefined as any;

// Both should error - both expressions are guaranteed to be "oops"
const foo = x?.y ?? `oops` ?? "";
//~^ ERROR: Right operand of ?? is unreachable because the left operand is never nullish.
const bar = (x?.y ?? `oops`) ?? "";
//~^ ERROR: Right operand of ?? is unreachable because the left operand is never nullish.

// Additional test cases with various levels of nesting
const baz = ((x?.y ?? `oops`)) ?? "";
//~^ ERROR: Right operand of ?? is unreachable because the left operand is never nullish.
const qux = (((x?.y ?? `oops`))) ?? "";
//~^ ERROR: Right operand of ?? is unreachable because the left operand is never nullish.

// Test with different types
const str1 = ("literal") ?? "fallback";
//~^ ERROR: Right operand of ?? is unreachable because the left operand is never nullish.
const str2 = (("nested")) ?? "fallback";
//~^ ERROR: Right operand of ?? is unreachable because the left operand is never nullish.
const nested = ("a" ?? "b") ?? "c";
//~^ ERROR: Right operand of ?? is unreachable because the left operand is never nullish.
//~| ERROR: Right operand of ?? is unreachable because the left operand is never nullish.

const a: string | undefined = undefined;
const b: string = a ?? "default";

function f(a: {
    b: string;
    c?: never;
} | {
    c: string;
    b?: never;
}) {
  const details: string = a.b ?? a.c;
}
