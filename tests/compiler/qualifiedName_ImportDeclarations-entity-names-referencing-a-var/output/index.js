// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/qualifiedName_ImportDeclarations-entity-names-referencing-a-var.ts`, Apache-2.0 License
var Alpha = {};
(function (Alpha) {

  var x = 100;
  Alpha.x = x
  
})(Alpha);

var x = Alpha.x;