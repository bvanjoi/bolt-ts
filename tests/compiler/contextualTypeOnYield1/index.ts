// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/contextualTypeOnYield1.ts`, Apache-2.0 License

//@compiler-options: target=es6

type FuncOrGeneratorFunc = () => (number | Generator<(arg: number) => void, any, void>)

const f: FuncOrGeneratorFunc = function*() {
  yield (num) => console.log(num); // `num` should be inferred to have type `number`.
}