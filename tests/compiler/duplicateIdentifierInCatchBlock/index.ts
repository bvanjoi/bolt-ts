// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/duplicateIdentifierInCatchBlock.ts`, Apache-2.0 License

//@compiler-options: target=es2015
var v;
try { } catch (e) {
    function v() { }
}

function w() { }
try { } catch (e) {
    var w;
    //~^ ERROR: Duplicate identifier 'w'.
}

try { } catch (e) {
    var x;
    function x() { } // error
    function e() { } // error
    var p: string;
    var p: number; // error
    //~^ ERROR: Subsequent variable declarations must have the same type. Variable 'p' must be of type 'string', but here has type 'number'.
}