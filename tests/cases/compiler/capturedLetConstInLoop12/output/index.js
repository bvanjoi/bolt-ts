// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/capturedLetConstInLoop12.ts`, Apache-2.0 License
(function () {
  "use strict";
  for ( var i = 0; i < 4; i++) {
    (() => [i] = [i + 1])();
  }
})();
(function () {
  "use strict";
  for ( var i = 0; i < 4; i++) {
    (() => ({a: i} = {a: i + 1}))();
  }
})();