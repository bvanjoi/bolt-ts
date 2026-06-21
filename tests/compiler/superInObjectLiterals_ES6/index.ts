// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/superInObjectLiterals_ES6.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var obj = {
    __proto__: {
        method() {
        }
    },
    method() {
        super.method();
    },
    get prop() {
        super.method();
        return 10;
    },
    set prop(value) {
        super.method();
    },
    p1: function () {
        super.method();
        //~^ ERROR: 'super' can only be referenced in members of derived classes or object literal expressions.
    },
    p2: function f() {
        super.method();
        //~^ ERROR: 'super' can only be referenced in members of derived classes or object literal expressions.
    },
    p3: () => {
        super.method();
        //~^ ERROR: 'super' can only be referenced in members of derived classes or object literal expressions.
    }
};

class A {
    method() { }
}

class B extends A {
    f() {
        var obj = {
            __proto__: {
                method() {
                }
            },
            method() {
                super.method();
            },
            get prop() {
                super.method();
                return 10;
            },
            set prop(value) {
                super.method();
            },
            p1: function () {
                super.method();
        //~^ ERROR: 'super' can only be referenced in members of derived classes or object literal expressions.
            },
            p2: function f() {
                super.method();
        //~^ ERROR: 'super' can only be referenced in members of derived classes or object literal expressions.
            },
            p3: () => {
                super.method();
            }
        };
    }
}