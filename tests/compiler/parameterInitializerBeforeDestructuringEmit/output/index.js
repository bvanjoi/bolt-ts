function foobar({bar = {}, ...opts} = {}) {
  'use strict';
  'Some other prologue';
  opts.baz(bar);
}
class C {
  constructor({bar = {}, ...opts} = {}) {'use strict';
    'Some other prologue';
    opts.baz(bar);}
}