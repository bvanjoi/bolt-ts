// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declFileForFunctionTypeAsTypeParameter.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

class X<T> {
}
class C extends X<() => number> {
}
interface I extends X<() => number> {
}