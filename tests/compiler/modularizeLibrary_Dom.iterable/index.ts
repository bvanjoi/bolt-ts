// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/modularizeLibrary_Dom.iterable.ts`, Apache-2.0 License

//@compiler-options: skipLibCheck
//@compiler-options: lib=[es6,dom,dom.iterable]
//@compiler-options: target=es6
//@run-fail

for (const element of document.getElementsByTagName("a")) {
    element.href;
}
