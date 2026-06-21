// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionAssignment.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

function f(n: Function) { }
f(function () { });

interface foo {
    get(handler: (bar: number)=>void): void;
}

interface baz {
    get(callback: Function): number;
}

declare var barbaz: baz;
declare var test: foo;

test.get(function (param) {
    var x = barbaz.get(function () { });
});

function f2(n: () => void) { }
f2(() => {
    var n = '';
    n = 4;
    //~^ ERROR: Type 'number' is not assignable to type 'string'.
});

function f3(a: { a: number; b: number; }) { }

f3({ a: 0, b: 0 });


function callb(lam:(l: number) => void );
function callb(lam:(n: string)=>void);
function callb(a) { }

callb((a) =>{ a.length; });
//~^ ERROR: Property 'length' does not exist on type 'number'.
