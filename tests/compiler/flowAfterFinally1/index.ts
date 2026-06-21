// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/flowAfterFinally1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strictNullChecks
//@run-fail

declare function openFile(): void
declare function closeFile(): void
declare function someOperation(): {}

var result: {}
openFile()
try {
  result = someOperation()
} finally {
  closeFile()
}

result // should not error here