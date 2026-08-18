// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/strictNullLogicalAndOr.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strictNullChecks

let sinOrCos = Math.random() < .5;
let choice = sinOrCos && Math.sin || Math.cos;

choice(Math.PI);

function sq(n?: number): number {
  const r = n !== undefined && n*n || 0;
  return r;
}

sq(3);
