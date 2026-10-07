// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/parameterInitializerBeforeDestructuringEmit.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: noImplicitUseStrict
//@compiler-options: alwaysStrict
//@run-fail
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