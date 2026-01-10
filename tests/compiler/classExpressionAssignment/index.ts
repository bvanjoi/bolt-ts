// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/classExpressionAssignment.ts`, Apache-2.0 License

interface A {
  prop: string;
}

// This is invalid
const A: {new(): A} = class {}
//~^ ERROR: Property 'prop' is missing.

const B: {new(): A} = class {
  prop = "hello";
};
