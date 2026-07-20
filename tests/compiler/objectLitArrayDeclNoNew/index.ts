// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/objectLitArrayDeclNoNew.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: lib=[es5]

declare var console;
"use strict";
namespace Test {
    export interface IState {
    }

    export interface IToken {
    }

    export interface ILineTokens {
        tokens: IToken[];
        endState: IState;
    }

    export class Gar {
        public moo: number = 0;
    }

    export function bug(): ILineTokens {
      var state:IState= null;
      return {
       tokens: Gar[],//IToken[],  // Missing new. Correct syntax is: tokens: new IToken[]
       //~^ ERROR: An element access expression should take an argument.
       endState: state
      };
     }
    }
} //~ ERROR: Declaration or statement expected.