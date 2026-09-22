// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/modularizeLibrary_Worker.asynciterable.ts`, Apache-2.0 License

//@compiler-options: skipLibCheck
//@compiler-options: lib=[es2018,webworker,webworker.asynciterable]
//@compiler-options: target=es2018
//@run-fail

navigator.storage.getDirectory().then(async directory => {
    for await (const [key, handle] of directory) {
        handle.kind;
    }
});