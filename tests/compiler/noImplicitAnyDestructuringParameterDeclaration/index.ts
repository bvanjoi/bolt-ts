// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noImplicitAnyDestructuringParameterDeclaration.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitAny

function f1([a], {b}, c, d) { // error
  //~^ ERROR: Binding element 'a' implicitly has an 'any' type.
  //~| ERROR: Binding element 'a' implicitly has an 'any' type.
  //~| ERROR: Binding element 'b' implicitly has an 'any' type.
  //~| ERROR: Binding element 'b' implicitly has an 'any' type.
  //~| ERROR: Parameter 'c' implicitly has an 'any' type.
  //~| ERROR: Parameter 'd' implicitly has an 'any' type.
}
function f2([a = undefined], {b = null}, c = undefined, d = null) { // error
}
function f3([a]: [any], {b}: { b: any }, c: any, d: any) {
}
function f4({b}: { b }, x: { b }) { // error in type instead
  //~^ ERROR: Member 'b' implicitly has an 'any' type.
  //~| ERROR: Member 'b' implicitly has an 'any' type.
}
function f5([a1] = [undefined], {b1} = { b1: null }, c1 = undefined, d1 = null) { // error
}