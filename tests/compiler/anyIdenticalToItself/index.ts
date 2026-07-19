// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/anyIdenticalToItself.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

function foo(x: any);
//~^ ERROR: This overload signature is not compatible with its implementation
function foo(x: any);
function foo(x: any, y: number) { }

class C {
    get X(): any {
        var y: any;
        return y;
    }
    set X(v: any) {
    }
}
