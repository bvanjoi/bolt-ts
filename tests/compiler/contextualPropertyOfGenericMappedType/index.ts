// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/contextualPropertyOfGenericMappedType.ts`, Apache-2.0 License

//@compiler-options: target=esnext
//@compiler-options: noImplicitAny
//@run-fail

declare function f<T extends object>(data: T, handlers: { [P in keyof T]: (value: T[P], prop: P) => void; }): void;
f({ data: 0 }, { data(value, key) {} });

