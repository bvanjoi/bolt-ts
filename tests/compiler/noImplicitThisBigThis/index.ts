// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noImplicitThisBigThis.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitThis
//@compiler-options: declaration

function createObj() {
    return {
        func1() {
            return this;
        },
        func2() {
            return this;
        },
        func3() {
            return this;
        }
    };
}

function createObjNoCrash() {
    return {
        func1() {
            return this;
        },
        func2() {
            return this;
        },
        func3() {
            return this;
        },
        func4() {
            return this;
        },
        func5() {
            return this;
        },
        func6() {
            return this;
        },
        func7() {
            return this;
        },
        func8() {
            return this;
        },
        func9() {
            return this;
        }
    };
}
