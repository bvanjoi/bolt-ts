// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/contextualSignatureInstantiationWithTypeParameterConstrainedToOuterTypeParameter.ts`, Apache-2.0 License

function f<T>() {
    function g<U extends T>(u: U): U { return null }
    //~^ ERROR: Type 'null' is not assignable to type 'U'.
    return g;
}
var h: <V, W>(v: V, func: (v: V) => W) => W;
var x = h("", f<string>()); // Call should succeed and x should be string. All type parameters should be instantiated to string
//~^ ERROR: Variable 'h' is used before being assigned.
//~| ERROR: Variable 'h' is used before being assigned.
var y: string = h("", f<string>());
//~^ ERROR: Variable 'h' is used before being assigned.
var z: number = h("", f<string>());
//~^ ERROR: Type 'string' is not assignable to type 'number'
//~| ERROR: Variable 'h' is used before being assigned.
