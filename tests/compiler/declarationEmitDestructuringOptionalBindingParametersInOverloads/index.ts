// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declarationEmitDestructuringOptionalBindingParametersInOverloads.ts`, Apache-2.0 License

//@compiler-options: strict=false
//@compiler-options: target=es2015
//@compiler-options: declaration

function foo([x, y, z] ?: [string, number, boolean]);
function foo(...rest: any[]) {
}

function foo2( { x, y, z }?: { x: string; y: number; z: boolean });
function foo2(...rest: any[]) {

}