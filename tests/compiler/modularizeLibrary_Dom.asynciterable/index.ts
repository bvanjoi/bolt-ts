// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/modKmodularizeLibrary_Dom.asynciterableeyword.ts`, Apache-2.0 License

//@compiler-options: skipLibCheck
//@compiler-options: lib=[es2018,dom,dom.asynciterable]
//@compiler-options: target=es2018
//@run-fail

navigator.storage.getDirectory().then(async directory => {
    for await (const [key, handle] of directory) {
        handle.kind;
    }
});
