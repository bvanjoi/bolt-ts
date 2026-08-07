// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/arrayAssignmentTest5.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace Test {
    interface IState {
    }
    interface IToken {
        startIndex: number;
    }
    interface IStateToken extends IToken {
        state: IState;
    }
    interface ILineTokens {
        tokens: IToken[];
        endState: IState;
    }
    interface IAction {
    }
    interface IMode {
        onEnter(line:string, state:IState, offset:number):IAction;
        tokenize(line:string, state:IState, includeStates:boolean):ILineTokens;
    }
    export class Bug implements IMode {
        public onEnter(line:string, state:IState, offset:number):IAction {
          //~^ ERROR: Function lacks ending return statement and return type does not include 'undefined'.
            var lineTokens:ILineTokens= this.tokenize(line, state, true);
            var tokens:IStateToken[]= lineTokens.tokens;
            //~^ ERROR: Property 'state' is missing.
            //~| ERROR: Property 'state' is missing.
            if (tokens.length === 0) {
                return this.onEnter(line, tokens, offset);        // <== this should produce an error since onEnter can not be called with (string, IStateToken[], offset)
            }
        }
        public tokenize(line:string, state:IState, includeStates:boolean):ILineTokens {
            return null;
            //~^ ERROR: Type 'null' is not assignable to type 'ILineTokens'.
        }
    }
}
