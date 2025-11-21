// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/optionalParamReferencingOtherParams3.ts`, Apache-2.0 License

function right(a = b, b = a) {
//~^ ERROR: Parameter 'a' cannot reference identifier 'b' declared after it.
    a;
    b;
}