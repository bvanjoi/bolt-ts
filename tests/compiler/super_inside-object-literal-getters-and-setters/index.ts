// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/super_inside-object-literal-getters-and-setters.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

namespace ObjectLiteral {
    var ThisInObjectLiteral = {
        _foo: '1',
        get foo(): string {
            return super._foo;
            //~[target=ES5]^ ERROR: 'super' is only allowed in members of object literal expressions when option 'target' is 'ES2015' or higher.
        },
        set foo(value: string) {
            super._foo = value;
            //~[target=ES5]^ ERROR: 'super' is only allowed in members of object literal expressions when option 'target' is 'ES2015' or higher.
        },
        test: function () {
            return super._foo;
            //~^ ERROR: 'super' can only be referenced in members of derived classes or object literal expressions.
        }
    }
}

class F { public test(): string { return ""; } }
class SuperObjectTest extends F {
    public testing() {
        var test = {
            get F() {
                return super.test();
            //~[target=ES5]^ ERROR: 'super' is only allowed in members of object literal expressions when option 'target' is 'ES2015' or higher.
            }
        };
    }
}

