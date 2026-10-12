// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/capturedLetConstInLoop14.ts`, Apache-2.0 License

//@compiler-options: strict
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

function use(v: number) {}

function foo(x: number) {
  var v = 1;
  do {
    let x = v;
    var v;
    //~^ ERROR: Subsequent variable declarations must have the same type. Variable 'v' must be of type 'number', but here has type 'any'. 
    var v = 2;
    () => x + v;
  } while (false);

  use(v);
}
