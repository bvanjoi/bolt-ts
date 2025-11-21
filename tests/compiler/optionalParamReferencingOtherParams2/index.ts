// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/optionalParamReferencingOtherParams2.ts`, Apache-2.0 License

var a = 1;
function strange(x = a, y = b) { 
//~^ ERROR: Parameter 'y' cannot reference identifier 'b' declared after it.
    var b = "";
    return y;
}