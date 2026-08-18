// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionAndInterfaceWithSeparateErrors.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function Foo(s: string);
//~^ ERROR: This overload signature is not compatible with its implementation signature.
//~| ERROR: 'Foo', which lacks return-type annotation, implicitly has an 'any' return type.
function Foo(n: number) { }

interface Foo {
    [s: string]: string;
    prop: number;
    //~^ ERROR: Property 'prop' of type 'number' is not assignable to 'string' index type 'string'.
}