// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/capturedLetConstInLoop2.ts`, Apache-2.0 License
// ========let
function foo0(x) {
  for ( var x of []) {
    var a = arguments.length;
    (function () {
      return x + a
    });
    (() => x + a);
  }
}
function foo0_1(x) {
  for ( var x in []) {
    var a = arguments.length;
    (function () {
      return x + a
    });
    (() => x + a);
  }
}
function foo1(x) {
  for ( var x = 0; x < 1; ++x) {
    var a = arguments.length;
    (function () {
      return x + a
    });
    (() => x + a);
  }
}
function foo2(x) {
  while (1 === 1) {
    var a = arguments.length;
    (function () {
      return x + a
    });
    (() => x + a);
  }
}
function foo3(x) {
  do {
    var x;
    var a = arguments.length;
    (function () {
      return x + a
    });
    (() => x + a);
  } while (1 === 1)
}
function foo4(x) {
  for ( var y = 0; y < 1; ++y) {
    var a = arguments.length;
    var x = 1;
    (function () {
      return x + a
    });
    (() => x + a);
  }
}
function foo5(x) {
  for ( var x = 0, y = 1; x < 1; ++x) {
    var a = arguments.length;
    (function () {
      return x + y + a
    });
    (() => x + y + a);
  }
}
function foo6(x) {
  while (1 === 1) {
    var x, y;
    var a = arguments.length;
    (function () {
      return x + y + a
    });
    (() => x + y + a);
  }
}
function foo7(x) {
  do {
    var x, y;
    var a = arguments.length;
    (function () {
      return x + y + a
    });
    (() => x + y + a);
  } while (1 === 1)
}
function foo8(x) {
  for ( var y = 0; y < 1; ++y) {
    var x = 1;
    var a = arguments.length;
    (function () {
      return x + y + a
    });
    (() => x + y + a);
  }
}
function foo0_c(x) {
  for ( var x of []) {
    var a = arguments.length;
    (function () {
      return x + a
    });
    (() => x + a);
  }
}
function foo0_1_c(x) {
  for ( var x in []) {
    var a = arguments.length;
    (function () {
      return x + a
    });
    (() => x + a);
  }
}
function foo1_c(x) {
  for ( var x = 0; x < 1; ) {
    var a = arguments.length;
    (function () {
      return x + a
    });
    (() => x + a);
  }
}
function foo2_c(x) {
  while (1 === 1) {
    var a = arguments.length;
    (function () {
      return x + a
    });
    (() => x + a);
  }
}
function foo3_c(x) {
  do {
    var x = 1;
    var a = arguments.length;
    (function () {
      return x + a
    });
    (() => x + a);
  } while (1 === 1)
}
function foo4_c(x) {
  for ( var y = 0; y < 1; ) {
    var a = arguments.length;
    var x = 1;
    (function () {
      return x + a
    });
    (() => x + a);
  }
}
function foo5_c(x) {
  for ( var x = 0, y = 1; x < 1; ) {
    var a = arguments.length;
    (function () {
      return x + y + a
    });
    (() => x + y + a);
  }
}
function foo6_c(x) {
  while (1 === 1) {
    var x = 1, y = 1;
    var a = arguments.length;
    (function () {
      return x + y + a
    });
    (() => x + y + a);
  }
}
function foo7_c(x) {
  do {
    var x = 1, y = 1;
    var a = arguments.length;
    (function () {
      return x + y + a
    });
    (() => x + y + a);
  } while (1 === 1)
}
function foo8_c(x) {
  for ( var y = 0; y < 1; ) {
    var x = 1;
    var a = arguments.length;
    (function () {
      return x + y + a
    });
    (() => x + y + a);
  }
}