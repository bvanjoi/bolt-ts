// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/baseTypeWrappingInstantiationChain.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class CBaseBase<T3> {
    constructor(x: Parameter<T3>) { }
}

class CBase<T2> extends CBaseBase<Wrapper<T2>> {

}

class Parameter<T4> {
    method(t: T4) { }
}

class Wrapper<T5> {
    property: T5;
    //~^ ERROR: Property 'property' has no initializer and is not definitely assigned in the constructor.
}

class C<T1> extends CBase<T1> {
    public works() {
        new CBaseBase<Wrapper<T1>>(this);
    }
    public alsoWorks() {
        new CBase<T1>(this); // Should not error, parameter is of type Parameter<Wrapper<T1>>
    }

    public method(t: Wrapper<T1>) { }
}
