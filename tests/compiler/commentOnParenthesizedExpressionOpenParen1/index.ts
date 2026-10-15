// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/commentOnParenthesizedExpressionOpenParen1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var j;
var f: () => any;
<any>( /* Preserve */ j = f());
//~^ ERROR: Variable 'f' is used before being assigned.