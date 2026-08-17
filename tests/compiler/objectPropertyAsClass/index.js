// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/objectPropertyAsClass.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: allowJs
//@compiler-options: noEmit
//@compiler-options: checkJs

const a1 = {
    foo() {
        this.x = 0;
    }
}

const a2 = {
    foo: function() {
        this.x = 0;
    }
}

const b1 = {
    /** @class */
    foo() {
        this.x = 0;
    }
}

const b2 = {
    /** @class */
    foo: function() {
        this.x = 0;
    }
}
