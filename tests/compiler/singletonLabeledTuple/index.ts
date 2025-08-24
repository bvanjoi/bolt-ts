// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/singletonLabeledTuple.ts`, Apache-2.0 License

type AliasOptional = [p?: number]

// literal type vs type alias
type Literal = [p?: number] extends [unknown] ? true : false // Expect `Literal` to be `false`
type Alias = AliasOptional extends [unknown] ? true : false     // Expect `Alias` to be `false`

// labeled tuple vs normal tuple
type Labeled = [p?: number] extends [unknown] ? true : false   // Expect `Labeled` to be `false`
type Normal = [number?] extends [unknown] ? true : false       // Expect `Normal` to be `false`


type AliasRest = [...p: number[]];

type LiteralRest = [...p: number[]] extends [unknown] ? true : false; // Expect `LiteralRest` to be `false`
type AliasedRest = AliasRest extends [unknown] ? true : false; // Expect `AliasedRest` to be `false`
type NormalRest = [...number[]] extends [unknown] ? true : false; // Expect `NormalRest` to be `false`

let a0: Literal = false;
let a1: Alias = false;
let a2: Labeled = false;
let a3: Normal = false;
let a4: LiteralRest = false;
let a5: AliasedRest = false;
let a6: NormalRest = false;