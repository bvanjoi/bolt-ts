// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/literalIntersectionYieldsLiteral.ts`, Apache-2.0 License

const x: { type: string } & { type: "string" } = { type: "string" };
const y: { type: number } & { type: 42 } = { type: 123 };
//~^ ERROR: Type '123' is not assignable to type '42'.