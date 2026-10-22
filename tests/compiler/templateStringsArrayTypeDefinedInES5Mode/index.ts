// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/templateStringsArrayTypeDefinedInES5Mode.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

class TemplateStringsArray {
}

function f(x: TemplateStringsArray, y: number, z: number) {
}

f({}, 10, 10);
//~[target=ES5]^ ERROR: Type '{ }' is missing the following properties from type 'TemplateStringsArray': raw, length, and 12 more.
//~[target=ES2015]^^ ERROR: Type '{ }' is missing the following properties from type 'TemplateStringsArray': raw, length, and 19 more.

f `abcdef${ 1234 }${ 5678 }ghijkl`;