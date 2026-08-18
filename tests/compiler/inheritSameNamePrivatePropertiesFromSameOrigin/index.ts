// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inheritSameNamePrivatePropertiesFromSameOrigin.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class B {
    private x: number;
    //~^ ERROR: Property 'x' has no initializer and is not definitely assigned in the constructor.
}
class C extends B { }

class C2 extends B { }

interface A extends C, C2 { // ok
    y: string;
}