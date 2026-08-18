// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/nestedObjectRest.ts`, Apache-2.0 License
var x, y;
[{
  ...x  
}] = [{
  abc: 1  
}];
for ( [{
  ...y  
}] of [[{
  abc: 1  
}]]) ;