// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/commentsOnObjectLiteral1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: removeComments=false
var Person = makeClass( //~ERROR: Cannot find name 'makeClass'.
   /** 
     @scope Person 
   */ 
   {
   } 
);