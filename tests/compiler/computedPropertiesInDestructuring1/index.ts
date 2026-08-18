// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/computedPropertiesInDestructuring1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

// destructuring in variable declarations
let foo = "bar";
let {[foo]: bar} = {bar: "bar"};
//~^ ERROR: Type '{ bar: string; }' has no matching index signature for type 'string'.

let {["bar"]: bar2} = {bar: "bar"};

let foo2 = () => "bar";
let {[foo2()]: bar3} = {bar: "bar"};
//~^ ERROR: Type '{ bar: string; }' has no matching index signature for type 'string'.

let [{[foo]: bar4}] = [{bar: "bar"}];
//~^ ERROR: Type '{ bar: string; }' has no matching index signature for type 'string'.
let [{[foo2()]: bar5}] = [{bar: "bar"}];
//~^ ERROR: Type '{ bar: string; }' has no matching index signature for type 'string'.

function f1({["bar"]: x}: { bar: number }) {}
function f2({[foo]: x}: { bar: number }) {}
//~^ ERROR: Type '{ bar: number; }' has no matching index signature for type 'string'.
function f3({[foo2()]: x}: { bar: number }) {}
//~^ ERROR: Type '{ bar: number; }' has no matching index signature for type 'string'.
function f4([{[foo]: x}]: [{ bar: number }]) {}
//~^ ERROR: Type '{ bar: number; }' has no matching index signature for type 'string'.
function f5([{[foo2()]: x}]: [{ bar: number }]) {}
//~^ ERROR: Type '{ bar: number; }' has no matching index signature for type 'string'.

// report errors on type errors in computed properties used in destructuring
let [{[foo()]: bar6}] = [{bar: "bar"}];
//~^ ERROR: This expression is not callable.
//~| ERROR: Type 'any' cannot be used as an index type.
let [{[foo.toExponential()]: bar7}] = [{bar: "bar"}];
//~^ ERROR: Property 'toExponential' does not exist on type 'string'.
//~| ERROR: Type 'any' cannot be used as an index type.

// destructuring assignment
({[foo]: bar} = {bar: "bar"});
//~^ ERROR: Type '{ bar: string; }' has no matching index signature for type 'string'.

({["bar"]: bar2} = {bar: "bar"});

({[foo2()]: bar3} = {bar: "bar"});
//~^ ERROR: Type '{ bar: string; }' has no matching index signature for type 'string'.

[{[foo]: bar4}] = [{bar: "bar"}];
//~^ ERROR: Type '{ bar: string; }' has no matching index signature for type 'string'.
[{[foo2()]: bar5}] = [{bar: "bar"}];
//~^ ERROR: Type '{ bar: string; }' has no matching index signature for type 'string'.

[{[foo()]: bar4}] = [{bar: "bar"}];
//~^ ERROR: This expression is not callable.
//~| ERROR: Type 'any' cannot be used as an index type.
[{[(1 + {})]: bar4}] = [{bar: "bar"}];
//~^ ERROR: Type 'any' cannot be used as an index type.
//~| ERROR: Operator '+' cannot be applied to types '1' and '{ }'.

