// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typePredicatesOptionalChaining1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

type X = {
  y?: {
    z?: string;
  };
};
const x: X = {
  y: {},
};
// type guard
function isNotNull<A>(x: A): x is NonNullable<A> {
  return x !== null && x !== undefined;
}
// function which I want to call in the result of the expression
function title(str: string) {
  return str.length > 0 ? "Dear " + str : "Dear nobody";
}

isNotNull(x?.y?.z) ? title(x.y.z) : null; // should not error

if (isNotNull(x?.y?.z)) {
  // TODO: let a: {z: string} = x.y; // should not error?
  let a: { z?: string } = x.y;
}
