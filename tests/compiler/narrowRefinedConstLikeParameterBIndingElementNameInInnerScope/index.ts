// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/narrowRefinedConstLikeParameterBIndingElementNameInInnerScope.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

function ff({ a, b }: { a: string | undefined, b: () => void }) {
  if (a !== undefined) {
    b = () => {
      const x: string = a;
    }
  }
}
