// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/duplicateObjectLiteralProperty_computedNameNegative1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

function bar(props: { x?: string; y?: string }) {
  const { x = "", y = "" } = props;
  return {
    [x]: 1,
    [y]: 2,
  };
}

function foo({ x = "", y = "" }: { x?: string; y?: string }) {
  return {
    [x]: 1,
    [y]: 2,
  };
}
