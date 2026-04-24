// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/wellKnownSymbolExpando.ts`, Apache-2.0 License

//@compiler-options: target=esnext
//@compiler-options: strict=false
//@compiler-options: noEmit

function f() {}
f[Symbol.iterator] = function() {}
