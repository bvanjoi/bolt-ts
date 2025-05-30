// From `github.com/microsoft/TypeScript/blob/v5.8.3/tests/cases/compiler/contextualTypingOfObjectLiterals.ts`, Apache-2.0 License

var obj1: { [x: string]: string; };
var obj2 = {x: ""};
obj1 = {}; // Ok
obj1 = obj2; // Error - indexer doesn't match

function f(x: { [s: string]: string }) { }

f({}); // Ok
f(obj1); // Ok
f(obj2); // Error - indexer doesn't match