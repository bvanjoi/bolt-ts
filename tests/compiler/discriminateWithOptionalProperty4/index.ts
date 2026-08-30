// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/discriminateWithOptionalProperty4.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@[exactOptionalPropertyTypes=true]  compiler-options: exactOptionalPropertyTypes
//@[exactOptionalPropertyTypes=false] compiler-options: exactOptionalPropertyTypes=false
//@compiler-options: noEmit

// https://github.com/microsoft/TypeScript/issues/55566

export function main(a: string[] | undefined) {
  const z = a ? { a } : { b: ["there"] };

  z.a //
    ? z.a.toString()
    : z.b.toString();

  const zWorkAround:
    | { a: string[]; b?: undefined }
    | { b: string[]; a?: undefined } = z;

  zWorkAround.a ? zWorkAround.a.toString() : zWorkAround.b.toString();

  "a" in z ? z.a.toString() : z.b.toString();
  //~[exactOptionalPropertyTypes=false]^ ERROR: 'z.a' is possibly 'undefined'.
}
