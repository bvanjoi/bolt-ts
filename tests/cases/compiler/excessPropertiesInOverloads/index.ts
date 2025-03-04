// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/excessPropertiesInOverloads.ts`, Apache-2.0 License

declare function fn(a: { x: string }): void;
declare function fn(a: { y: string }): void;
fn({ z: 3, a: 3 });
//~^ ERROR: No overload matches this call.