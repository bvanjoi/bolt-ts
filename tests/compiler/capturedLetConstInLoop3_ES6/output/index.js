function foo0(x) {
  for ( var x of []) {
    var v = x;
    (function () {
      return x + v
    });
    (() => (x + v));
  }
  use(v);
}
function foo0_1(x) {
  for ( var x in []) {
    var v = x;
    (function () {
      return x + v
    });
    (() => (x + v));
  }
  use(v);
}
function foo1(x) {
  for ( var x = 0; x < 1; ++x) {
    var v = x;
    (function () {
      return x + v
    });
    (() => (x + v));
  }
  use(v);
}
function foo2(x) {
  while (1 === 1) {
    var x = 1;
    var v = x;
    (function () {
      return x + v
    });
    (() => (x + v));
  }
  use(v);
}
function foo3(x) {
  do {
    var x;
    var v;
    (function () {
      return x + v
    });
    (() => (x + v));
  } while (1 === 1)
  use(v);
}
function foo4(x) {
  for ( var y = 0; y < 1; ++y) {
    var v = y;
    var x = 1;
    (function () {
      return x + v
    });
    (() => (x + v));
  }
  use(v);
}
function foo5(x) {
  for ( var x = 0, y = 1; x < 1; ++x) {
    var v = x;
    (function () {
      return x + y + v
    });
    (() => (x + y + v));
  }
  use(v);
}
function foo6(x) {
  while (1 === 1) {
    var x, y;
    var v = x;
    (function () {
      return x + y + v
    });
    (() => (x + y + v));
  }
  use(v);
}
function foo7(x) {
  do {
    var x, y;
    var v = x;
    (function () {
      return x + y + v
    });
    (() => (x + y + v));
  } while (1 === 1)
  use(v);
}
function foo8(x) {
  for ( var y = 0; y < 1; ++y) {
    var x = 1;
    var v = x;
    (function () {
      return x + y + v
    });
    (() => (x + y + v));
  }
  use(v);
}
function foo0_c(x) {
  for ( var x of []) {
    var v = x;
    (function () {
      return x + v
    });
    (() => (x + v));
  }
  use(v);
}
function foo0_1_c(x) {
  for ( var x in []) {
    var v = x;
    (function () {
      return x + v
    });
    (() => (x + v));
  }
  use(v);
}
function foo1_c(x) {
  for ( var x = 0; x < 1; ) {
    var v = x;
    (function () {
      return x + v
    });
    (() => (x + v));
  }
  use(v);
}
function foo2_c(x) {
  while (1 === 1) {
    var x = 1;
    var v = x;
    (function () {
      return x + v
    });
    (() => (x + v));
  }
  use(v);
}
function foo3_c(x) {
  do {
    var x = 1;
    var v;
    (function () {
      return x + v
    });
    (() => (x + v));
  } while (1 === 1)
  use(v);
}
function foo4_c(x) {
  for ( var y = 0; y < 1; ) {
    var v = y;
    var x = 1;
    (function () {
      return x + v
    });
    (() => (x + v));
  }
  use(v);
}
function foo5_c(x) {
  for ( var x = 0, y = 1; x < 1; ) {
    var v = x;
    (function () {
      return x + y + v
    });
    (() => (x + y + v));
  }
  use(v);
}
function foo6_c(x) {
  while (1 === 1) {
    var x = 1, y = 1;
    var v = x;
    (function () {
      return x + y + v
    });
    (() => (x + y + v));
  }
  use(v);
}
function foo7_c(x) {
  do {
    var x = 1, y = 1;
    var v = x;
    (function () {
      return x + y + v
    });
    (() => (x + y + v));
  } while (1 === 1)
  use(v);
}
function foo8_c(x) {
  for ( var y = 0; y < 1; ) {
    var x = 1;
    var v = x;
    (function () {
      return x + y + v
    });
    (() => (x + y + v));
  }
  use(v);
}