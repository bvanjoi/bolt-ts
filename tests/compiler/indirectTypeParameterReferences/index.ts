// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/indirectTypeParameterReferences.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@run-fail

type B = {b: string}

const flowtypes = <A>(b: B) => {
  type Combined = A & B

  const combined = (fn: (combined: Combined) => void) => null
  const literal = (fn: (aPlusB: A & B) => void) => null

  return {combined, literal}
}

const {combined, literal} = flowtypes<{a: string}>({b: 'b-value'})

literal(aPlusB => {
  aPlusB.b
  aPlusB.a
})

combined(comb => {
  comb.b
  comb.a
})

// Repro from #19091

declare function f<T>(a: T): { a: typeof a };
let n: number = f(2).a;
