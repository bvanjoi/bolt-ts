// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/strictModeUseContextualKeyword.ts`, Apache-2.0 License
"use strict"
var as = 0;
function foo(as: string) { }
class C {
    public as() { }
}
function F() {
    function as() { }
}
function H() {
    let {as} = { as: 1 };
}
