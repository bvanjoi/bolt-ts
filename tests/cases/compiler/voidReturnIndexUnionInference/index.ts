// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/voidReturnIndexUnionInference.ts`, Apache-2.0 License

export function safeInvoke<A1, R>(
  func: ((arg1: A1) => R) | null | undefined,
  arg1: A1
): R | undefined {
  if (func) {
      return func(arg1);
  } else {
      return undefined;
  }
}

interface Props {
  onFoo?(value: string): boolean;
  onBar?(value: string): void;
}

function bad<P extends Props>(props: Readonly<P>) {
  safeInvoke(props.onFoo, "blah");
  safeInvoke(props.onBar, "blah");

  let a: string = safeInvoke(props.onFoo, "blah");
  //~^ ERROR: Type 'undefined | false | true' is not assignable to type 'string'.
  let b: string = safeInvoke(props.onBar, "blah");
  //~^ ERROR: Type 'undefined | void' is not assignable to type 'string'.
}
