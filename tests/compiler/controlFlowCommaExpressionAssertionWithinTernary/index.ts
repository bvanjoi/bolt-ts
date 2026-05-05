// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/controlFlowCommaExpressionAssertionWithinTernary.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

declare function assert(value: any): asserts value;

function foo2(param: number | null | undefined): number | null {
    const val = param !== undefined;
    return val ? (assert(param !== undefined), param) : null;
    // ^^^^^ Still typed as number | null | undefined
}