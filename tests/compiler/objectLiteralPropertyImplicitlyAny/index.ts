// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/objectLiteralPropertyImplicitlyAny.ts`, Apache-2.0 License

//@compiler-options: target=esnext
//@compiler-options: noImplicitAny

const foo = Symbol.for("foo");
const o = { [foo]: undefined };
