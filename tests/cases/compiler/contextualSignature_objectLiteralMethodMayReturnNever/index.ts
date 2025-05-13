// From `github.com/microsoft/TypeScript/blob/v5.8.3/tests/cases/compiler/contextualSignature_objectLiteralMethodMayReturnNever.ts`, Apache-2.0 License

//@ run-fail

interface I { m(): number; }
const o: I = { m() { throw new Error("not implemented"); } };

const d0 = { m() { throw new Error("not implemented"); } };

let a0: never = d0.m();

const d1 = { m() { } };

// TODO: check endFlowNodes of function node
// let a1: never = d1.m();