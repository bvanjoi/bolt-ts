// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/transformParenthesizesConditionalSubexpression.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: lib=[es5]

namespace M
{
    export class Test
    {
      private name = "hello";
      public setName = function(value: string): void {
	  (function () {
	      name=value;
        //~^ ERROR: Cannot find name 'name'.
	  })();
      }
      public getName = function(): string {
          return name;
        //~^ ERROR: Cannot find name 'name'.
      }
    }
}


