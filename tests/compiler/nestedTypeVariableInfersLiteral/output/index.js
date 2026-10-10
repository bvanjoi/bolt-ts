// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/nestedTypeVariableInfersLiteral.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@run-fail
var directUnionSingle = direct('z');
var directUnionArray = direct(['z', 'y']);
var nestedSingle = nested({
  fields: 'z'  
});
var nestedUnionSingle = nestedUnion({
  fields: 'z'  
});
var nestedUnionArray = nestedUnion({
  fields: ['z', 'y']  
});
hasZField(directUnionSingle)// ok
;
hasZField(directUnionArray)// ok
;
hasZField(nestedSingle)// ok
;
hasZField(nestedUnionSingle)// ok
;
hasZField(nestedUnionArray);