// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/computedPropertyBindingElementDeclarationNoCrash1.ts`, Apache-2.0 License

//@compiler-options: strict
//@compiler-options: target=esnext
//@compiler-options: lib=[esnext]
//@compiler-options: noEmit

// https://github.com/microsoft/TypeScript/issues/61351

export type State = {
  a: number;
  b: string;
};

export class Test {
  setState(state: State) {}
  test = (e: any) => {
    for (const [key, value] of Object.entries(e)) {
      this.setState({
        [key]: value,
        //~^ ERROR: Property 'a' is missing.
        //~| ERROR: Property 'b' is missing.
      });
    }
  };
}