// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedLocalsAndParameters.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

export { };

// function declaration paramter
function f(a) {
  //~^ ERROR: 'a' is declared but its value is never read.
}
f(0);

// function expression paramter
var fexp = function (a) {
  //~^ ERROR: 'a' is declared but its value is never read.
};

fexp(0);

// arrow function paramter
var farrow = (a) => {
  //~^ ERROR: 'a' is declared but its value is never read.
  //~| ERROR: 'farrow' is declared but its value is never read.
};

class C {
  //~^ ERROR: 'C' is declared but never used.
    // Method declaration paramter
    method(a) {
  //~^ ERROR: 'a' is declared but its value is never read.
    }
    // Accessor declaration paramter
    set x(v: number) {
  //~^ ERROR: 'v' is declared but its value is never read.
    }
}

var E = class {
  //~^ ERROR: 'E' is declared but its value is never read.
    // Method declaration paramter
    method(a) {
  //~^ ERROR: 'a' is declared but its value is never read.
    }
    // Accessor declaration paramter
    set x(v: number) {
  //~^ ERROR: 'v' is declared but its value is never read.
    }
}

var o = {
    // Object literal method declaration paramter
    method(a) {
  //~^ ERROR: 'a' is declared but its value is never read.
    },
    // Accessor declaration paramter
    set x(v: number) {
  //~^ ERROR: 'v' is declared but its value is never read.
    }
};

o;

// in a for..in statment
for (let i in o) {
  //~^ ERROR: 'i' is declared but its value is never read.
}

// in a for..of statment
for (let i of [1, 2, 3]) {
  //~^ ERROR: 'i' is declared but its value is never read.
}

// in a for. statment
for (let i = 0, n; i < 10; i++) {
  //~^ ERROR: 'n' is declared but its value is never read.
}

// in a block

const condition = false;
if (condition) {
    const c = 0;
  //~^ ERROR: 'c' is declared but its value is never read.
}

// in try/catch/finally
try {
    const a = 0;
  //~^ ERROR: 'a' is declared but its value is never read.
}
catch (e) {
    const c = 1;
  //~^ ERROR: 'c' is declared but its value is never read.
}
finally {
    const c = 0;
  //~^ ERROR: 'c' is declared but its value is never read.
}


// in a namespace
namespace N {
  //~^ ERROR: 'N' is declared but its value is never read.
    var x;
  //~^ ERROR: 'x' is declared but its value is never read.
}

for (let x: y) {
  //~^ ERROR: Expected ','.
  //~| ERROR: A destructuring declaration must have an initializer.
  //~| ERROR: Cannot find name 'y'.
    z(x);
  //~^ ERROR: Expected ','.
  //~| ERROR: Expected '}'.
  //~| ERROR: Expected ','.
  //~| ERROR: Expected ';'.
  //~| ERROR: 'z' is declared but its value is never read.
}
  //~^ ERROR: Expression expected.
  //~| ERROR: Expected ')'.
  //~| ERROR: Expression expected.
  //~| ERROR: Declaration or statement expected.
