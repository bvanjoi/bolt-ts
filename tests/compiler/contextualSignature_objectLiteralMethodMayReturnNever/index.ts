// From `github.com/microsoft/TypeScript/blob/v5.8.3/tests/cases/compiler/contextualSignature_objectLiteralMethodMayReturnNever.ts`, Apache-2.0 License

interface I { m(): number; }
const o: I = { m() { throw new Error("not implemented"); } };

const d0 = { m() { throw new Error("not implemented"); } };

let a0: never = d0.m();

const d1 = { m() { } };

let a1: never = d1.m();
//~^ ERROR: Type 'void' is not assignable to type 'never'.