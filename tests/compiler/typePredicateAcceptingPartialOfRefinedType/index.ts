// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typePredicateAcceptingPartialOfRefinedType.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit
//@compiler-options: exactOptionalPropertyTypes

interface Test {
  testy?: string;
}

interface Options {
  test: Test['testy'];
}

declare function includesAllRequiredOptions(options: Partial<Options>): options is Options;
