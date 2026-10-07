// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedLocalsAndParametersDeferred.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters
export {  }
function defered(a) {
  return a();
}
// function declaration paramter
function f(a) {
  defered(() => {
    a;
  });
}
f(0);
// function expression paramter
var fexp = function (a) {
  defered(() => {
    a;
  });
};
fexp(1);
// arrow function paramter
var farrow = (a) => {
  defered(() => {
    a;
  });
};
farrow(2);
var prop1;
class C {
  // Method declaration paramter
  method(a) {
    defered(() => {
      a;
    });
  }
  // Accessor declaration paramter
  set x(v) {
    defered(() => {
      v;
    });
  }
  // in a property initalizer
  p = defered(() => {
    prop1;
  });
}
new C();
var prop2;
var E = class {
  // Method declaration paramter
  method(a) {
    defered(() => {
      a;
    });
  }
  // Accessor declaration paramter
  set x(v) {
    defered(() => {
      v;
    });
  }
  // in a property initalizer
  p = defered(() => {
    prop2;
  });
};
new E();
var o = // Object literal method declaration paramter
{
  method(a) {
    defered(() => {
      a;
    });
  }// Accessor declaration paramter
  ,
  set x(v) {
    defered(() => {
      v;
    });
  },
  // in a property initalizer
  p: defered(() => {
    prop1;
  })  
};
o;
// in a for..in statment
for ( var i in o) {
  defered(() => {
    i;
  });
}
// in a for..of statment
for ( var i of [1, 2, 3]) {
  defered(() => {
    i;
  });
}
// in a for. statment
for ( var i = 0; i < 10; i++) {
  defered(() => {
    i;
  });
}
// in a block
var condition = false;
if (condition) {
  var c = 0;
  defered(() => {
    c;
  });
}

// in try/catch/finally
try {
  var a = 0;
  defered(() => {
    a;
  });
} catch (e) {
  var c = 1;
  defered(() => {
    c;
  });
}finally {
  var c = 0;
  defered(() => {
    c;
  });
}
// in a namespace
var N = {};
(function (N) {

  var x;
  
  defered(() => {
    x;
  });
  
})(N);
N;