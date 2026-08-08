// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/templateStringsArrayTypeRedefinedInES6Mode.ts`, Apache-2.0 License

//@compiler-options: target=es6

class TemplateStringsArray {
}

function f(x: TemplateStringsArray, y: number, z: number) {
}

f({}, 10, 10);
//~^ ERROR: Type '{ }' is missing the following properties from type 'TemplateStringsArray': raw, length, and 19 more.

f `abcdef${ 1234 }${ 5678 }ghijkl`;