// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/commaOperator1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: allowUnreachableCode=false

var v1 = ((1, 2, 3), 4, 5, (6, 7));
//~^ ERROR: Left side of comma operator is unused and has no side effects.
//~| ERROR: Left side of comma operator is unused and has no side effects.
//~| ERROR: Left side of comma operator is unused and has no side effects.
//~| ERROR: Left side of comma operator is unused and has no side effects.
//~| ERROR: Left side of comma operator is unused and has no side effects.
//~| ERROR: Left side of comma operator is unused and has no side effects.
function f1() {
    var a = 1;
    return a, v1, a;
//~^ ERROR: Left side of comma operator is unused and has no side effects.
//~| ERROR: Left side of comma operator is unused and has no side effects.
}
