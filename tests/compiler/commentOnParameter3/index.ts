// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/commentOnParameter3.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

function commentedParameters(
a /* parameter a */, 
b /* parameter b */,
/* extra comment */
) { }
