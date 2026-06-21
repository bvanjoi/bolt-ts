// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/destructuringTempOccursAfterPrologue.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function test(p: any) {
    'use strict';
    'use strong';
    p = { prop: p } = p;
}