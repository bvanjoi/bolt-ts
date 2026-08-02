// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declFileGenericClassWithGenericExtendedClass.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

interface IFoo {
    baz: Baz;
}
class Base<T> { }
class Derived<T> extends Base<T> { }
interface IBar<T> {
    derived: Derived<T>;
}
class Baz implements IBar<Baz> {
    derived: Derived<Baz>;
    //~^ ERROR: Property 'derived' has no initializer and is not definitely assigned in the constructor.
}