// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/restParameterTypeInstantiation.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

// Repro from #33823

interface TestGeneric<TG> {
  f: string
  g: TG
}

const removeF = <TX>({ f, ...rest }: TestGeneric<TX>) => {
  return rest
}

const result: number = removeF<number>({ f: '', g: 3 }).g
