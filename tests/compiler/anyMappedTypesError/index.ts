// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/anyMappedTypesError.ts`, Apache-2.0 License

//@compiler-options: noImplicitAny

type Foo = {[P in "bar"]};
//~^ ERROR: Mapped object type implicitly has an 'any' template type.