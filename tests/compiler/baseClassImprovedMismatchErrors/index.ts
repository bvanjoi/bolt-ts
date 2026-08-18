// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/baseClassImprovedMismatchErrors.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class Base {
    n: Base | string;
    //~^ ERROR: Property 'n' has no initializer and is not definitely assigned in the constructor.
    fn() {
        return 10;
    }
}
class Derived extends Base {
    n: Derived | string;
    //~^ ERROR: Property 'n' in type 'Derived<Derived>' is not assignable to the same property in base type 'Base<Derived>'.
    //~| ERROR: Property 'n' has no initializer and is not definitely assigned in the constructor.
    fn() {
    //~^ ERROR: Property 'fn' in type 'Derived<Derived>' is not assignable to the same property in base type 'Base<Derived>'
        return 10 as number | string;
    }
}
class DerivedInterface implements Base {
    n: DerivedInterface | string;
    //~^ ERROR: Property 'n' in type 'DerivedInterface<DerivedInterface>' is not assignable to the same property in base type 'Base<DerivedInterface>'.
    //~| ERROR: Property 'n' has no initializer and is not definitely assigned in the constructor.
    fn() {
    //~^ ERROR: Property 'fn' in type 'DerivedInterface<DerivedInterface>' is not assignable to the same property in base type 'Base<DerivedInterface>'.
        return 10 as number | string;
    }
}