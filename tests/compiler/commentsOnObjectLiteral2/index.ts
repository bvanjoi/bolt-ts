// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/commentsOnObjectLiteral2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: removeComments=false

var Person = makeClass( //~ERROR: Cannot find name 'makeClass'.
   { 
       /** 
        This is just another way to define a constructor. 
        @constructs 
        @param {string} name The name of the person. 
        */ 
       initialize: function(name) { 
           this.name = name; 
       } /* trailing comment 1*/, 
   } 
);