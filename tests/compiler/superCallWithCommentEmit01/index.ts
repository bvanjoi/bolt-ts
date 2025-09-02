// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/superCallWithCommentEmit01.ts`, Apache-2.0 License

class A {
    constructor(public text: string) { }
}

class B extends A {
    constructor(text: string) {
        // this is subclass constructor
        super(text)
     }
}