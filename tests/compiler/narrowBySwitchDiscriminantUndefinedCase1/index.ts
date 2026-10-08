// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/narrowByParenthesizedSwitchExpression.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@[a]     compiler-options: exactOptionalPropertyTypes
//@[a]     compiler-options: noUncheckedIndexedAccess
//@[b]     compiler-options: exactOptionalPropertyTypes=false
//@[b]     compiler-options: noUncheckedIndexedAccess
//@[c]     compiler-options: exactOptionalPropertyTypes
//@[c]     compiler-options: noUncheckedIndexedAccess=false
//@[d]     compiler-options: exactOptionalPropertyTypes=false
//@[d]     compiler-options: noUncheckedIndexedAccess=false
//@compiler-options: noEmit

// https://github.com/microsoft/TypeScript/issues/57999

interface A {
  optionalProp?: "hello";
}

function func(arg: A) {
  const { optionalProp } = arg;

  switch (optionalProp) {
    case undefined:
      return undefined;
    case "hello":
      return "hello";
    default:
      assertUnreachable(optionalProp);
  }
}

function func2() {
  const optionalProp = ["hello" as const][Math.random()];

  switch (optionalProp) {
    case undefined:
      return undefined;
    case "hello":
      return "hello";
    default:
      assertUnreachable(optionalProp);
  }
}

function assertUnreachable(_: never): never {
  throw new Error("Unreachable path taken");
}
