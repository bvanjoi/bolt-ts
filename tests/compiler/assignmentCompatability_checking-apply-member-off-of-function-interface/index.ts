// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/assignmentCompatability_checking-apply-member-off-of-function-interface.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

interface Applicable {
    apply(blah: any); // also works for 'apply'
}

var x: Applicable;

// Should fail
x = '';
//~^ ERROR: Type 'string' is not assignable to type 'Applicable'.
x = [''];
//~^ ERROR: Property 'apply' is missing.
x = 4;
//~^ ERROR: Type 'number' is not assignable to type 'Applicable'.
x = {};
//~^ ERROR: Property 'apply' is missing.

// Should work
function f() { };
x = f;

function fn(c: Applicable) { }

// Should Fail
fn('');
//~^ ERROR: Argument of type 'string' is not assignable to parameter of type 'Applicable'.
fn(['']);
//~^ ERROR: Property 'apply' is missing.
fn(4);
//~^ ERROR: Argument of type 'number' is not assignable to parameter of type 'Applicable'.
fn({});
//~^ ERROR: Property 'apply' is missing.


// Should work
fn(a => { });
