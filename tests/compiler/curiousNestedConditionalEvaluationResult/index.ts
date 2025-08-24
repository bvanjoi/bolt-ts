// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/curiousNestedConditionalEvaluationResult.ts`, Apache-2.0 License

type Hmm = [0] extends [infer T, any?] ?
    [T, [0] extends [T] ? true : false]
    : never

let a: Hmm = [0, true]