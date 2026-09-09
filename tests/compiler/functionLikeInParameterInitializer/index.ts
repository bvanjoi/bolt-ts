// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionLikeInParameterInitializer.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

// error
export function bar(func = () => foo) {
  //~[target=ES2015]^ ERROR: Cannot find name 'foo'.
    let foo = "in";
}
// error
export function baz1(func = { f() { return foo } }) {
  //~[target=ES2015]^ ERROR: Cannot find name 'foo'.
    let foo = "in";
}

// error
export function baz2(func = function () { return foo }) {
  //~[target=ES2015]^ ERROR: Cannot find name 'foo'.
    let foo = "in";
}

// error
export function baz3(func = class { x = foo }) {
  //~[target=ES2015]^ ERROR: Cannot find name 'foo'.
    let foo = "in";
}
