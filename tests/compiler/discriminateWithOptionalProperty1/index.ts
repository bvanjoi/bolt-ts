// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/discriminateWithOptionalProperty1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit
//@[exactOptionalPropertyTypes=true]  compiler-options: exactOptionalPropertyTypes
//@[exactOptionalPropertyTypes=false] compiler-options: exactOptionalPropertyTypes=false

type Box<T> = { done?: false } | { done: true; value: T };

declare const box: Box<number>;

if (box.done) {
  box.value;
}
