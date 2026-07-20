// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/arrayAssignmentTest6.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace Test {
    interface IState {
    }
    interface IToken {
        startIndex: number;
    }
    interface ILineTokens {
        tokens: IToken[];
        endState: IState;
    }
    interface IMode {
        tokenize(line:string, state:IState, includeStates:boolean):ILineTokens;
    }
    export class Bug implements IMode {
        public tokenize(line:string, tokens:IToken[], includeStates:boolean):ILineTokens {
            return null;
            //~^ ERROR: Type 'null' is not assignable to type 'ILineTokens'.
        }
    }    
}
