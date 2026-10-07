// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/contextualTypeObjectSpreadExpression.ts`, Apache-2.0 License
var i;
i = {
  ...{
      a: 'a'    
  }  
};
i = {
  a: 'a'  
};
var j;
j = {
  ...{
      a: 'a'    
  }  
};
j = {
  a: 'a'  
};