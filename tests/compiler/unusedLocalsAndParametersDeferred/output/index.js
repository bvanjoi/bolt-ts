export {  }
function defered(a) {
  return a();
}
function f(a) {
  defered(() => {
    a;
  });
}
f(0);
var fexp = function (a) {
  defered(() => {
    a;
  });
};
fexp(1);
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
  set x(v) {
    defered(() => {
      v;
    });
  }
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
  set x(v) {
    defered(() => {
      v;
    });
  }
  p = defered(() => {
    prop2;
  });
};
new E();
var o = {
  method(a) {
    defered(() => {
      a;
    });
  },
  set x(v) {
    defered(() => {
      v;
    });
  },
  p: defered(() => {
    prop1;
  })  
};
o;
for ( var i in o) {
  defered(() => {
    i;
  });
}
for ( var i of [1, 2, 3]) {
  defered(() => {
    i;
  });
}
for ( var i = 0; i < 10; i++) {
  defered(() => {
    i;
  });
}
var condition = false;
if (condition) {
  var c = 0;
  defered(() => {
    c;
  });
}

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
var N = {};
(function (N) {

  var x;
  
  defered(() => {
    x;
  });
  
})(N);
N;