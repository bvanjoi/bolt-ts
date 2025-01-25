// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/restArgAssignmentCompat.ts`, Apache-2.0 License

function f(...x: number[]) {
  x.forEach((n, i) => void ('item ' + i + ' = ' + n));
}
function g(x: number[], y: string) { }

var n = g;
n = f;
//~^ ERROR: Type '(x: number[]) => void' is not assignable to type '(x: number[], y: string) => void'.
n([4], 'foo');
