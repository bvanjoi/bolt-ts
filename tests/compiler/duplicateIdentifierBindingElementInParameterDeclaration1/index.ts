// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/duplicateIdentifierBindingElementInParameterDeclaration1.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

function f0(a, [a, [b]], {b}) { }
//~^ ERROR: Duplicate identifier 'a'.
//~| ERROR: Duplicate identifier 'b'.
//~| ERROR: Parameter 'a' implicitly has an 'any' type.
//~| ERROR: Binding element 'a' implicitly has an 'any' type.
//~| ERROR: Binding element 'a' implicitly has an 'any' type.
//~| ERROR: Binding element 'a' implicitly has an 'any' type.
//~| ERROR: Binding element 'a' implicitly has an 'any' type.
//~| ERROR: Binding element 'a' implicitly has an 'any' type.
//~| ERROR: Binding element 'b' implicitly has an 'any' type.
//~| ERROR: Binding element 'b' implicitly has an 'any' type.
//~| ERROR: Binding element 'b' implicitly has an 'any' type.
//~| ERROR: Binding element 'b' implicitly has an 'any' type.
//~| ERROR: Binding element 'b' implicitly has an 'any' type.
//~| ERROR: Binding element 'b' implicitly has an 'any' type.
//~| ERROR: Binding element 'b' implicitly has an 'any' type.
function f1([a, a]) { }
//~^ ERROR: Duplicate identifier 'a'.
//~| ERROR: Binding element 'a' implicitly has an 'any' type.
//~| ERROR: Binding element 'a' implicitly has an 'any' type.
//~| ERROR: Binding element 'a' implicitly has an 'any' type.
//~| ERROR: Binding element 'a' implicitly has an 'any' type.
//~| ERROR: Binding element 'a' implicitly has an 'any' type.
//~| ERROR: Binding element 'a' implicitly has an 'any' type.
//~| ERROR: Binding element 'a' implicitly has an 'any' type.
//~| ERROR: Binding element 'a' implicitly has an 'any' type.
function f2({b}, {b}) { }
//~^ ERROR: Duplicate identifier 'b'.
//~| ERROR: Binding element 'b' implicitly has an 'any' type.
//~| ERROR: Binding element 'b' implicitly has an 'any' type.
//~| ERROR: Binding element 'b' implicitly has an 'any' type.
//~| ERROR: Binding element 'b' implicitly has an 'any' type.
function f3([c,[c],[[c]]]) { }
//~^ ERROR: Duplicate identifier 'c'.
//~| ERROR: Duplicate identifier 'c'.
//~| ERROR: Binding element 'c' implicitly has an 'any' type.
//~| ERROR: Binding element 'c' implicitly has an 'any' type.
//~| ERROR: Binding element 'c' implicitly has an 'any' type.
//~| ERROR: Binding element 'c' implicitly has an 'any' type.
//~| ERROR: Binding element 'c' implicitly has an 'any' type.
//~| ERROR: Binding element 'c' implicitly has an 'any' type.
//~| ERROR: Binding element 'c' implicitly has an 'any' type.
//~| ERROR: Binding element 'c' implicitly has an 'any' type.
//~| ERROR: Binding element 'c' implicitly has an 'any' type.
//~| ERROR: Binding element 'c' implicitly has an 'any' type.
//~| ERROR: Binding element 'c' implicitly has an 'any' type.
//~| ERROR: Binding element 'c' implicitly has an 'any' type.
//~| ERROR: Binding element 'c' implicitly has an 'any' type.
//~| ERROR: Binding element 'c' implicitly has an 'any' type.
//~| ERROR: Binding element 'c' implicitly has an 'any' type.
//~| ERROR: Binding element 'c' implicitly has an 'any' type.
//~| ERROR: Binding element 'c' implicitly has an 'any' type.
//~| ERROR: Binding element 'c' implicitly has an 'any' type.
//~| ERROR: Binding element 'c' implicitly has an 'any' type.
//~| ERROR: Binding element 'c' implicitly has an 'any' type.
//~| ERROR: Binding element 'c' implicitly has an 'any' type.
//~| ERROR: Binding element 'c' implicitly has an 'any' type.
//~| ERROR: Binding element 'c' implicitly has an 'any' type.
//~| ERROR: Binding element 'c' implicitly has an 'any' type.
//~| ERROR: Binding element 'c' implicitly has an 'any' type.
//~| ERROR: Binding element 'c' implicitly has an 'any' type.
//~| ERROR: Binding element 'c' implicitly has an 'any' type.
function f4({d, d:{d}}) { }
//~^ ERROR: Duplicate identifier 'd'.
//~| ERROR: Binding element 'd' implicitly has an 'any' type.
//~| ERROR: Binding element 'd' implicitly has an 'any' type.
//~| ERROR: Binding element 'd' implicitly has an 'any' type.
//~| ERROR: Binding element 'd' implicitly has an 'any' type.
//~| ERROR: Binding element 'd' implicitly has an 'any' type.
//~| ERROR: Binding element 'd' implicitly has an 'any' type.
//~| ERROR: Binding element 'd' implicitly has an 'any' type.
//~| ERROR: Binding element 'd' implicitly has an 'any' type.
//~| ERROR: Binding element 'd' implicitly has an 'any' type.
//~| ERROR: Binding element 'd' implicitly has an 'any' type.
function f5({e, e: {e}}, {e}, [d,e, [[e]]], ...e) { }
//~^ ERROR: Duplicate identifier 'e'.
//~| ERROR: Duplicate identifier 'e'.
//~| ERROR: Duplicate identifier 'e'.
//~| ERROR: Duplicate identifier 'e'.
//~| ERROR: Duplicate identifier 'e'.
//~| ERROR: Binding element 'e' implicitly has an 'any' type.
//~| ERROR: Binding element 'e' implicitly has an 'any' type.
//~| ERROR: Binding element 'e' implicitly has an 'any' type.
//~| ERROR: Binding element 'e' implicitly has an 'any' type.
//~| ERROR: Binding element 'e' implicitly has an 'any' type.
//~| ERROR: Binding element 'e' implicitly has an 'any' type.
//~| ERROR: Binding element 'e' implicitly has an 'any' type.
//~| ERROR: Binding element 'e' implicitly has an 'any' type.
//~| ERROR: Binding element 'e' implicitly has an 'any' type.
//~| ERROR: Binding element 'e' implicitly has an 'any' type.
//~| ERROR: Binding element 'e' implicitly has an 'any' type.
//~| ERROR: Binding element 'e' implicitly has an 'any' type.
//~| ERROR: Binding element 'e' implicitly has an 'any' type.
//~| ERROR: Binding element 'e' implicitly has an 'any' type.
//~| ERROR: Binding element 'e' implicitly has an 'any' type.
//~| ERROR: Binding element 'e' implicitly has an 'any' type.
//~| ERROR: Binding element 'e' implicitly has an 'any' type.
//~| ERROR: Binding element 'e' implicitly has an 'any' type.
//~| ERROR: Binding element 'e' implicitly has an 'any' type.
//~| ERROR: Binding element 'e' implicitly has an 'any' type.
//~| ERROR: Binding element 'e' implicitly has an 'any' type.
//~| ERROR: Binding element 'e' implicitly has an 'any' type.
//~| ERROR: Binding element 'e' implicitly has an 'any' type.
//~| ERROR: Binding element 'e' implicitly has an 'any' type.
//~| ERROR: Binding element 'e' implicitly has an 'any' type.
//~| ERROR: Binding element 'e' implicitly has an 'any' type.
//~| ERROR: Binding element 'e' implicitly has an 'any' type.
//~| ERROR: Binding element 'e' implicitly has an 'any' type.
//~| ERROR: Binding element 'd' implicitly has an 'any' type.
//~| ERROR: Binding element 'd' implicitly has an 'any' type.
//~| ERROR: Binding element 'd' implicitly has an 'any' type.
//~| ERROR: Binding element 'd' implicitly has an 'any' type.
//~| ERROR: Binding element 'd' implicitly has an 'any' type.
//~| ERROR: Binding element 'd' implicitly has an 'any' type.
//~| ERROR: Binding element 'd' implicitly has an 'any' type.
//~| ERROR: Binding element 'd' implicitly has an 'any' type.
//~| ERROR: Rest parameter 'e' implicitly has an 'any[]' type.
function f6([f, ...f]) { }
//~^ ERROR: Duplicate identifier 'f'.
//~| ERROR: Binding element 'f' implicitly has an 'any' type.
//~| ERROR: Binding element 'f' implicitly has an 'any' type.
//~| ERROR: Binding element 'f' implicitly has an 'any' type.
//~| ERROR: Binding element 'f' implicitly has an 'any' type.
//~| ERROR: Binding element 'f' implicitly has an 'any' type.
//~| ERROR: Binding element 'f' implicitly has an 'any' type.
//~| ERROR: Binding element 'f' implicitly has an 'any' type.
//~| ERROR: Binding element 'f' implicitly has an 'any' type.
function f7(a, func = (a) => { return 1 }) { }  // not error
//~^ ERROR: Parameter 'a' implicitly has an 'any' type.
//~| ERROR: Parameter 'a' implicitly has an 'any' type.