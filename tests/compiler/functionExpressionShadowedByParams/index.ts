// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionExpressionShadowedByParams.ts`, Apache-2.0 License

//@compiler-options: target=es2015
function b1(b1: number) {
   b1.toPrecision(2); // should not error
   b1(12); // should error
   //~^ ERROR: This expression is not callable.
}


var x = {
   b: function b(b: number) {
      b.toPrecision(2); // should not error
      b.apply(null, null); // should error
      //~^ ERROR: Property 'apply' does not exist on type 'number'.
   }
};
