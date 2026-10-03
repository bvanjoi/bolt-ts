// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declarationEmitBindingPatterns.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: declaration

const k = ({x: z = 'y'}) => { }

var a;
function f({} = a, [] = a, { p: {} = a} = a) {
}