// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/reverseMappedContravariantInference.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@run-fail

declare function conforms<T>(source: { [K in keyof T]: (val: T[K]) => boolean }): (value: T) => boolean;
conforms({ foo: (v: string) => false })({ foo: "hello" });
