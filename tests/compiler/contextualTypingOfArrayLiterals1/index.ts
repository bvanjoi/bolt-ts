// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/contextualTypingOfArrayLiterals1.ts`, Apache-2.0 License

interface I {
   [x: number]: Date;
}

var x3: I = [new Date(), 1]; 
//~^ ERROR: Type 'number' is not assignable to type 'Date'.
var r2 = x3[1]; 
r2.getDate(); 
