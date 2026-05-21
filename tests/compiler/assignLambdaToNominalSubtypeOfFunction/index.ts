// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/assignLambdaToNominalSubtypeOfFunction.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

interface IResultCallback extends Function {
    x: number;
}

function fn(cb: IResultCallback) { }

fn((a, b) => true);
//~^ ERROR: Argument of type '(a: any, b: any) => boolean' is not assignable to parameter of type 'IResultCallback'.
fn(function (a, b) { return true; })
//~^ ERROR: Argument of type '(a: any, b: any) => boolean' is not assignable to parameter of type 'IResultCallback'.
