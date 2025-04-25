// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/templateLiteralConstantEvaluation.ts`, Apache-2.0 License

function fn<T>(arg: T): T {
  return arg;
}

const a = '1';
const b = a + ' 2';
const c = `${b} 3`;
const d = `${b} 3` as const;

fn(`${b} 3`);
let s: number = fn(`${b} 3`);
//~^ ERROR: Type 'string' is not assignable to type 'number'.

function f0() {
  do return `
`; while (false);
}