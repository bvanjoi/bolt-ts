// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/downlevelLetConst18.ts`, Apache-2.0 License

//@compiler-options: strict=false
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
//@compiler-options: allowUnreachableCode

'use strict'

for (let x; ;) {
    function foo() { x };
      //~[target=ES5]^ ERROR: Function declarations are not allowed inside blocks in strict mode when targeting 'ES5'.
}

for (let x; ;) {
    function foo1() { x };
      //~[target=ES5]^ ERROR: Function declarations are not allowed inside blocks in strict mode when targeting 'ES5'.
}

for (let x; ;) {
    (() => { x })();
}

for (const x = 1; ;) {
    (() => { x })();
}

for (let x; ;) {
    ({ foo() { x }})
}

for (let x; ;) {
    ({ get foo() { return x } })
}

for (let x; ;) {
    ({ set foo(v) { x } })
}

let a: 42 = '42';
//~^ ERROR: Type 'string' is not assignable to type '42'.