// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/autoLift3.ts`, Apache-2.0 License

class B {

    constructor() {
        function foo() {  }

        foo();

        var a = 0;
        var inner: any = (function() {
            var CScriptIO = (function() {
                var fso = 0

                return {
                    readFile: function(path: string): string {
                        return fso.toString();
                    }
                }
            })();
            return inner;
        })();
    }
}

var b = new B();

b.foo();
//~^ ERROR: Property 'foo' does not exist on type 'B'.


