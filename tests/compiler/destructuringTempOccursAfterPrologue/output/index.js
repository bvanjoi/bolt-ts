// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/destructuringTempOccursAfterPrologue.ts`, Apache-2.0 License
function test(p) {
  'use strict';
  'use strong';
  p = {
      prop: p    
  } = p;
}