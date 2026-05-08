// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/defineVariables_useDefineForClassFields.ts`, Apache-2.0 License

//@compiler-options: target=ESNext
//@compiler-options: strict=false
//@compiler-options: useDefineForClassFields

const a = () => b()
const b = () => null
a()