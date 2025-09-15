// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/ArrowFunctionExpression1.ts`, Apache-2.0 License

var v = (public x: string) => { };
//~^ ERROR: A parameter property is only allowed in a constructor implementation.

var j = () => ({ value: true ? void 0 : 3 });