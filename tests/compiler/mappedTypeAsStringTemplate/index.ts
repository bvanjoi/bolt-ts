// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/mappedTypeAsStringTemplate.ts`, Apache-2.0 License

//@compiler-options: target=esnext

function foo<T extends { [K in keyof T as `${Extract<K, string>}y`]: number }>(foox: T) { }

const c = { x: 1 };

foo(c);
//~^ ERROR: Property 'xy' is missing.
