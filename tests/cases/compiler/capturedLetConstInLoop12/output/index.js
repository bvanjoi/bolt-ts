(function () {
  'use strict';
  for ( var i = 0; i < 4; i++) {
    (() => [i] = [i + 1])();
  }
})();
(function () {
  'use strict';
  for ( var i = 0; i < 4; i++) {
    (() => ({a: i} = {a: i + 1}))();
  }
})();