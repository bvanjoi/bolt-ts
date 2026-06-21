// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inferSecondaryParameter.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

interface Ib { m(test: string, fn: Function); }

var b: Ib = { m: function (test: string, fn: Function) { } };

b.m("test", function (bug) {
    var a: number = bug;
});