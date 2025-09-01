// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/emitThisInSuperMethodCall.ts`, Apache-2.0 License

class User {
    sayHello() {
    }
}

class RegisteredUser extends User {
    f() {
        () => {
            function inner() {
                super.sayHello();
                //~^ ERROR: 'super' can only be referenced in members of derived classes or object literal expressions.
            }
        };
    }
    g() {
        function inner() {
            () => {
                super.sayHello();
                //~^ ERROR: 'super' can only be referenced in members of derived classes or object literal expressions.
            }
        }
    }
    h() {
        function inner() {
            super.sayHello();
            //~^ ERROR: 'super' can only be referenced in members of derived classes or object literal expressions.
        }
    }
}
