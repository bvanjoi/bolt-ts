// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/thisBinding2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitThis
//@compiler-options: noImplicitAny

class C {
 x!: number;
    constructor() {
        this.x = (() => {
   var x = 1;
   return this.x;
  })();
  this.x = function() {
   var x = 1;
   return this.x;
   //~^ ERROR: 'this' implicitly has type 'any' because it does not have a type annotation.
  }();
    }  
}
declare function setTimeout(expression: any, msec?: number, language?: any): number;
var messenger = {
    message: "Hello World",
    start: function () {
        return setTimeout(() => { var x = this.message; }, 3000);
    }
};
