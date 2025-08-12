// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/objectLiteral1.ts`, Apache-2.0 License

var v30 = {a:1, b:2};

var v31 = {
  123: 123,
  [123.456]: 123.456
};

const a0: number = v31[123];
const a1: number = v31[123.456];
const a2: number = v31['123'];
const a3: number = v31['123.456'];
