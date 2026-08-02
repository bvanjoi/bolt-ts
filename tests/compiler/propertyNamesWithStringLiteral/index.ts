// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/propertyNamesWithStringLiteral.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class _Color {
    a: number; r: number; g: number; b: number;
    //~^ ERROR: Property 'a' has no initializer and is not definitely assigned in the constructor.
    //~| ERROR: Property 'r' has no initializer and is not definitely assigned in the constructor.
    //~| ERROR: Property 'g' has no initializer and is not definitely assigned in the constructor.
    //~| ERROR: Property 'b' has no initializer and is not definitely assigned in the constructor.
}

interface NamedColors {
    azure: _Color;
    "blue": _Color;
    "pale blue": _Color;
}
namespace Color {
    export var namedColors: NamedColors;
}
var a = Color.namedColors["azure"];
var a = Color.namedColors.blue; // Should not error
var a = Color.namedColors["pale blue"]; // should not error
