// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/webworkerIterable.ts`, Apache-2.0 License

//@compiler-options: skipLibCheck
//@compiler-options: lib=[es2020,webworker,webworker.iterable]
//@compiler-options: target=es2020
//@run-fail

// This API is only in webworker
importScripts("")

// This should not raise a compiler error
const f = new FormData()
for (const element of f) {
  element.length
}
