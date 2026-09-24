// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedParametersWithUnderscore.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

function f(a, _b, c, ___, d,e___, _f) {
  //~^ ERROR: 'a' is declared but its value is never read.
  //~| ERROR: 'c' is declared but its value is never read.
  //~| ERROR: 'd' is declared but its value is never read.
  //~| ERROR: 'e___' is declared but its value is never read.
}


function f2({_a, __b}) {
  //~^ ERROR: All destructured elements are unused.
}

function f3([_a, ,__b]) {
}

function f4(...arg) {
  //~^ ERROR: 'arg' is declared but its value is never read.
}

function f5(..._arg) {
}

function f6(arg?, _arg?) {
  //~^ ERROR: 'arg' is declared but its value is never read.
}

var f7 = _ => undefined;

var f8 = function (_) { };