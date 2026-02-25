// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/arrayBindingPatternOmittedExpressions.ts`, Apache-2.0 License

//@compiler-options: target=ES6
//@run-fail

var results: string[];


{
    let [, b, , a] = results;
    let x = {
        a,
        b
    }
}


function f([, a, , b, , , , s, , , ] = results) {
    a = s[1];
    b = s[2];
}