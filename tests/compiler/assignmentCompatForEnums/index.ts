// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/assignmentCompatForEnums.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

enum TokenType { One, Two };

var list = {};


function returnType(): TokenType { return null; }

function foo() {
    var x = returnType();

    var x: TokenType = list['one'];
}

