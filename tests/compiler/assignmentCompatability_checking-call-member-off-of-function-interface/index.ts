// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/assignmentCompatability_checking-call-member-off-of-function-interface.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

interface Callable {
    call(blah: any); // also works for 'apply'
}

var x: Callable;

// Should fail
x = '';
//~^ ERROR: Type 'string' is not assignable to type 'Callable'.
x = [''];
//~^ ERROR: Property 'call' is missing.
x = 4;
//~^ ERROR: Type 'number' is not assignable to type 'Callable'.
x = {};
//~^ ERROR: Property 'call' is missing.

// Should work
function f() { };
x = f;

function fn(c: Callable) { }

// Should Fail
fn('');
//~^ ERROR: Argument of type 'string' is not assignable to parameter of type 'Callable'.
fn(['']);
//~^ ERROR: Property 'call' is missing.
fn(4);
//~^ ERROR: Argument of type 'number' is not assignable to parameter of type 'Callable'.
fn({});
//~^ ERROR: Property 'call' is missing.


// Should work
fn(a => { });
