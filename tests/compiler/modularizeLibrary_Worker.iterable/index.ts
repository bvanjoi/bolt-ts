// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/modularizeLibrary_Worker.iterable.ts`, Apache-2.0 License

//@compiler-options: skipLibCheck
//@compiler-options: lib=[es6,webworker,webworker.iterable]
//@compiler-options: es6

for (const [key, entry] of new FormData()) {
    entry;
}
