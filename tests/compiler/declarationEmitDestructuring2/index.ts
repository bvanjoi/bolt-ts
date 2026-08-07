// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declarationEmitDestructuring2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: declaration

function f({x = 10, y: [a, b, c, d] = [1, 2, 3, 4]} = { x: 10, y: [2, 4, 6, 8] }) { }
function g([a, b, c, d] = [1, 2, 3, 4]) { }
function h([a, [b], [[c]], {x = 10, y: [a, b, c], z: {a1, b1}}]){ }
//~^ ERROR: Duplicate identifier 'a'.
//~| ERROR: Duplicate identifier 'b'.
//~| ERROR: Duplicate identifier 'c'.
function h1([a, [b], [[c]], {x = 10, y = [1, 2, 3], z: {a1, b1}}]){ }


function h2({a}, {a}) {}
//~^ ERROR: Duplicate identifier 'a'.
function h3({b}, {a: b}) {}
//~^ ERROR: Duplicate identifier 'b'.
