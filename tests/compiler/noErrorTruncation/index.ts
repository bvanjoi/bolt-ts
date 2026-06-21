// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noErrorTruncation.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noErrorTruncation

type SomeLongOptionA = { someLongOptionA: string }
type SomeLongOptionB = { someLongOptionB: string }
type SomeLongOptionC = { someLongOptionC: string }
type SomeLongOptionD = { someLongOptionD: string }
type SomeLongOptionE = { someLongOptionE: string }
type SomeLongOptionF = { someLongOptionF: string }

const x: SomeLongOptionA  //~ERROR: Type 'number' is not assignable to type 'SomeLongOptionA | SomeLongOptionB | SomeLongOptionC | SomeLongOptionD | SomeLongOptionE | SomeLongOptionF'.
       | SomeLongOptionB
       | SomeLongOptionC
       | SomeLongOptionD
       | SomeLongOptionE
       | SomeLongOptionF = 42;
