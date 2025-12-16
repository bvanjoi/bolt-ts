// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/contextualSignatureInstantiationWithTypeParameterConstrainedToOuterTypeParameter.ts`, Apache-2.0 License

function f<T>() {
    function g<U extends T>(u: U): U { return null }
    return g;
}
var h: <V, W>(v: V, func: (v: V) => W) => W;
var x = h("", f<string>()); // Call should succeed and x should be string. All type parameters should be instantiated to string
var y: string = h("", f<string>());
var z: number = h("", f<string>());
//~^ ERROR: Type 'string' is not assignable to type 'number'