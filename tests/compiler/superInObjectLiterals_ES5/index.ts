// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/superInObjectLiterals_ES5.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

var obj = {
    __proto__: {
        method() {
        }
    },
    method() {
        super.method();
        //~[target=ES5]^ ERROR: 'super' is only allowed in members of object literal expressions when option 'target' is 'ES2015' or higher.
    },
    get prop() {
        super.method();
        //~[target=ES5]^ ERROR: 'super' is only allowed in members of object literal expressions when option 'target' is 'ES2015' or higher.
        return 10;
    },
    set prop(value) {
        super.method();
        //~[target=ES5]^ ERROR: 'super' is only allowed in members of object literal expressions when option 'target' is 'ES2015' or higher.
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
        //~[target=ES5]^ ERROR: 'super' is only allowed in members of object literal expressions when option 'target' is 'ES2015' or higher.
            },
            get prop() {
                super.method();
        //~[target=ES5]^ ERROR: 'super' is only allowed in members of object literal expressions when option 'target' is 'ES2015' or higher.
                return 10;
            },
            set prop(value) {
                super.method();
        //~[target=ES5]^ ERROR: 'super' is only allowed in members of object literal expressions when option 'target' is 'ES2015' or higher.
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