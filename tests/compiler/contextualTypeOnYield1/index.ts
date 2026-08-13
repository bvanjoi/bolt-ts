// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contextualTypeOnYield1.ts`, Apache-2.0 License

//@compiler-options: strict
//@compiler-options: target=es6

type FuncOrGeneratorFunc = () => (number | Generator<(arg: number) => void, any, void>)

const f: FuncOrGeneratorFunc = function*() {
  yield (num) => console.log(num); // `num` should be inferred to have type `number`.
}

const g: FuncOrGeneratorFunc = function*() {
  yield (num) => {
    const k: string = num;
    //~^ ERROR: Type 'number' is not assignable to type 'string'.
  }
}