// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/anyIdenticalToItself.ts`, Apache-2.0 License

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