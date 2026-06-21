// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/narrowByClauseExpressionInSwitchTrue9.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

interface IProps {
    one: boolean;
}

class Foo {
    mine: string = "";

    myMethod(x: IProps) {
        const { one } = x;
        switch (true) {
            case one:
                const a0: true = one;   
                const a1: false = one; //~ERROR: Type 'boolean' is not assignable to type 'false'.
                break;
            default:
                let x = this.mine;
        }
    }
}
