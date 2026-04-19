// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/objectBindingPatternContextuallyTypesArgument.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@run-fail

declare function id<T>(x: T): T;
const { f = (x: string) => x.length } = id({ f: x => x.charAt });
