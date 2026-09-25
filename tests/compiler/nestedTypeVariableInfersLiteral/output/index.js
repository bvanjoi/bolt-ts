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
hasZField(directUnionSingle);
hasZField(directUnionArray);
hasZField(nestedSingle);
hasZField(nestedUnionSingle);
hasZField(nestedUnionArray);