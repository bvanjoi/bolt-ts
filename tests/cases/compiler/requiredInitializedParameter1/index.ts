// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/requiredInitializedParameter1.ts`, Apache-2.0 License

function f1(a, b = 0, c) { }
function f2(a, b = 0, c = 0) { }
function f3(a, b = 0, c?) { }
function f4(a, b = 0, ...c) { }

f1(0, 1, 2);
f2(0, 1, 2);
f3(0, 1, 2);
f4(0, 1, 2);

f1(0, 1);
//~^ ERROR: Expected 3 arguments, but got 2.
f2(0, 1);
f3(0, 1);
f4(0, 1);

f1(0);
//~^ ERROR: Expected 3 arguments, but got 1.
f2(0);
f3(0);
f4(0);