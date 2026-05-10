// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/mappedTypeRecursiveInference2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

type MorphTuple = [string, "|>", any]

type validateMorph<def extends MorphTuple> = def[1] extends "|>"
    ? [validateDefinition<def[0]>, "|>", (In: def[0]) => unknown]
    : def

type validateDefinition<def> = def extends MorphTuple
    ? validateMorph<def>
    : {
          [k in keyof def]: validateDefinition<def[k]>
      }

declare function type<def>(def: validateDefinition<def>): def

const shallow = type(["ark", "|>", (x) => x.length])
const objectLiteral = type({ a: ["ark", "|>", (x) => x.length] })
const nestedTuple = type([["ark", "|>", (x) => x.length]])
