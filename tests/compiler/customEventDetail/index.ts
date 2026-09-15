// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/customEventDetail.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var x: CustomEvent;

// valid since detail is any
x.initCustomEvent('hello', true, true, { id: 12, name: 'hello' });
//~^ ERROR: Variable 'x' is used before being assigned.
var y = x.detail.name;
//~^ ERROR: Variable 'x' is used before being assigned.
