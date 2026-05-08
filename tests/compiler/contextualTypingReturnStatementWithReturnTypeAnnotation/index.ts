// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contextualTypingReturnStatementWithReturnTypeAnnotation.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

type PropOfRaw<T> = readonly T[] | "not-array" | "no-prop";

declare function isString(text: unknown): text is string;

declare function getPropFromRaw<T>(
  prop: "files" | "include" | "exclude" | "references",
  validateElement: (value: unknown) => boolean,
  elementTypeName: string
): PropOfRaw<T>;

function getSpecsFromRaw(
  prop: "files" | "include" | "exclude"
): PropOfRaw<string> {
  return getPropFromRaw(prop, isString, "string");
}