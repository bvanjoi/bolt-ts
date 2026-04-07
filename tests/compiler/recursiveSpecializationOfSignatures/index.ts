// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/recursiveSpecializationOfSignatures.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class S0<B, A> {
set S1(S2: S0<any,any>) {
}
constructor(public S17: S0<any, (S18) => A>) { }
}
