// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/binopAssignmentShouldHaveType.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: lib=[es5]

declare var console;
"use strict";
namespace Test {
 export class Bug {
  getName():string {
   return "name";
  }
  bug() {
   var name:string= null;
   if ((name= this.getName()).length > 0) {
    console.log(name);
   }
  }
 }
}

 

