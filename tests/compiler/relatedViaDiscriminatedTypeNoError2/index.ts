// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/relatedViaDiscriminatedTypeNoError2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@run-fail

type AObjOrBObj = { name: "A" } | { name: "B" };
type AOrBObj = { name: "A" | "B" };
type Generic<T extends AObjOrBObj> = T;

type T = Generic<AOrBObj>;

declare let x: AObjOrBObj;
declare let y: AOrBObj;
x = y;
y = x;
