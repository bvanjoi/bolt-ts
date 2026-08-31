// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/templateStringsArrayTypeNotDefinedES5Mode.ts`, Apache-2.0 License

//@[target=es5]     compiler-options: target=es5
//@[target=es2015]  compiler-options: target=es2015

function f(x: TemplateStringsArray, y: number, z: number) {
}

f({}, 10, 10);
//~[target=es5]^    ERROR: Type '{ }' is missing the following properties from type 'TemplateStringsArray': raw, length, and 12 more.
//~[target=es2015]^^ ERROR: Type '{ }' is missing the following properties from type 'TemplateStringsArray': raw, length, and 19 more.

f `abcdef${ 1234 }${ 5678 }ghijkl`;