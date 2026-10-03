// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declarationEmitDestructuringArrayPattern2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

var [x10, [y10, [z10]]] = [1, ["hello", [true]]];

var [x11 = 0, y11 = ""] = [1, "hello"];
var [a11, b11, c11] = [];
//~^ ERROR: Tuple type '[]' of length '0' has no element at index '0'.
//~| ERROR: Tuple type '[]' of length '0' has no element at index '1'.
//~| ERROR: Tuple type '[]' of length '0' has no element at index '2'.
var [a2, [b2, { x12, y12: c2 }]=["abc", { x12: 10, y12: false }]] = [1, ["hello", { x12: 5, y12: true }]];

var [x13, y13] = [1, "hello"];
var [a3, b3] = [[x13, y13], { x: x13, y: y13 }];
