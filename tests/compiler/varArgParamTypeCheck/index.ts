// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/varArgParamTypeCheck.ts`, Apache-2.0 License

function sequence(...sequences:{():void;}[]) {
}

function callback(clb:()=>void) {
}

sequence(
    function bar() {
    },
    function foo() {
        callback(()=>{
            this();
            //~^ ERROR: 'this' implicitly has type 'any' because it does not have a type annotation.
        });
    },
    function baz() {
        callback(()=>{
            this();
            //~^ ERROR: 'this' implicitly has type 'any' because it does not have a type annotation.
        });
    }
);
