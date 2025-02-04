// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/blockScopedBindingsReassignedInLoop1.ts`, Apache-2.0 License

declare function use(n: number): void;
declare function useString(s: string): void;
(function () {
  'use strict'
  for (let i = 0; i < 9; ++i) {
    (() => use(++i))();
    (() => useString(++i))();
    //~^ ERROR:  Argument of type 'number' is not assignable to parameter of type 'string'.
  }
})();
