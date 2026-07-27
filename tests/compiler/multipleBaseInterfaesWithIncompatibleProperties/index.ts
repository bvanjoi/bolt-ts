// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/multipleBaseInterfaesWithIncompatibleProperties.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface A<T>
{
    x: T
}

interface C extends A<string>, A<number> { }
//~^ ERROR: Interface 'C' cannot simultaneously extend types 'A<string>' and 'A<number>'.
