// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declarationEmitDestructuringWithOptionalBindingParameters.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

function foo([x,y,z]?: [string, number, boolean]) {
  //~^ ERROR: A binding pattern parameter cannot be optional in an implementation signature.
}
function foo1( { x, y, z }?: { x: string; y: number; z: boolean }) {
  //~^ ERROR: A binding pattern parameter cannot be optional in an implementation signature.
}
function foo2( {x, y, z}? ) {
  //~^ ERROR: A binding pattern parameter cannot be optional in an implementation signature.
}
function foo3( [x, y, z]? ) {
  //~^ ERROR: A binding pattern parameter cannot be optional in an implementation signature.
}