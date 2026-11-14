// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/jsxHash.tsx`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: jsx=preserve

var t02 = <a>{0}#</a>;
var t03 = <a>#{0}</a>;
var t04 = <a>#{0}#</a>;
var t05 = <a>#<i></i></a>;
var t06 = <a>#<i></i></a>;
var t07 = <a>#<i>#</i></a>;
var t08 = <a><i></i>#</a>;
var t09 = <a>#<i></i>#</a>;
var t10 = <a><i/>#</a>;
var t11 = <a>#<i/></a>;
var t12 = <a>#</a>;
