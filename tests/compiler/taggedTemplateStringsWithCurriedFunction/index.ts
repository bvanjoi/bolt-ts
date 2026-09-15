// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/taggedTemplateStringsWithCurriedFunction.ts`, Apache-2.0 License

//@compiler-options: strict=false
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

const f = _ => (..._) => "";

f({ ...{ x: 0 } })``;
f({ ...{ x: 0 } })`x`;
f({ ...{ x: 0 } })`x${f}x`;
f({ ...{ x: 0 }, y: (() => 1)() })``;
f({ x: (() => 1)(), ...{ y: 1 } })``;
