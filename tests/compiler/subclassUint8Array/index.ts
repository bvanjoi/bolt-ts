// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/subclassUint8Array.ts`, Apache-2.0 License

//@compiler-options: target=esnext
//@compiler-options: lib=[esnext]
//@compiler-options: noEmit

class CustomBuffer extends Uint8Array {
}
