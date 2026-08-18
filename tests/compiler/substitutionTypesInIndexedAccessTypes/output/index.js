// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/substitutionTypesInIndexedAccessTypes.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict
var boundaryResult = withBoundary({
  select: true  
});
var withoutBoundaryResult = withoutBoundary({
  select: true  
});