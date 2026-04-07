// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/discriminantUsingEvaluatableTemplateExpression.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strictNullChecks
//@run-fail

interface A { kind: 'A'; }
interface B { kind: 'B'; }

type C = A | B | undefined;

function never(_: never): never {
    throw new Error();
}

function useA(_: A): void { }
function useB(_: B): void { }

declare var c: C;

if (c !== undefined) {
    switch (c.kind) {
        case 'A': useA(c); break;
        case 'B': useB(c); break;
        default: never(c);
    }
}