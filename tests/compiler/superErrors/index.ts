// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/superErrors.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function foo() {
    // super in a non class context
    var x = super;
    //~^ ERROR: 'super' must be followed by an argument list or member access.
    //~| ERROR: Identifier expected.
    //~| ERROR: 'super' can only be referenced in members of derived classes or object literal expressions.
    var y = () => super;
    //~^ ERROR: 'super' must be followed by an argument list or member access.
    //~| ERROR: Identifier expected.
    //~| ERROR: 'super' can only be referenced in members of derived classes or object literal expressions.
    var z = () => () => () => super;
    //~^ ERROR: 'super' must be followed by an argument list or member access.
    //~| ERROR: Identifier expected.
    //~| ERROR: 'super' can only be referenced in members of derived classes or object literal expressions.
}

class User {
    name: string = "Bob";
    sayHello(): void {
        //console.log("Hello, " + this.name);
    }
}

class RegisteredUser extends User {
    name: string = "Frank";
    constructor() {
        super();

        // super call in an inner function in a constructor
        function inner() {
            super.sayHello();
            //~^ ERROR: 'super' can only be referenced in members of derived classes or object literal expressions.
        }

        // super call in a lambda in an inner function in a constructor 
        function inner2() {
            var x = () => super.sayHello();
            //~^ ERROR: 'super' can only be referenced in members of derived classes or object literal expressions.
        }

        // super call in a lambda in a function expression in a constructor 
        (function() { return () => super; })();
        //~^ ERROR: 'super' must be followed by an argument list or member access.
        //~| ERROR: Identifier expected.
        //~| ERROR: 'super' can only be referenced in members of derived classes or object literal expressions.
    }
    sayHello(): void {
        // super call in a method
        super.sayHello();

        // super call in a lambda in an inner function in a method
        function inner() {
            var x = () => super.sayHello();
            //~^ ERROR: 'super' can only be referenced in members of derived classes or object literal expressions
        }

        // super call in a lambda in a function expression in a constructor 
        (function() { return () => super; })();
        //~^ ERROR: 'super' must be followed by an argument list or member access.
        //~| ERROR: Identifier expected.
        //~| ERROR: 'super' can only be referenced in members of derived classes or object literal expressions.
    }
    static staticFunction(): void {
        // super in static functions
        var s = super;
        //~^ ERROR: 'super' must be followed by an argument list or member access.
        //~| ERROR: Identifier expected.
        var x = () => super;
        //~^ ERROR: 'super' must be followed by an argument list or member access.
        //~| ERROR: Identifier expected.
        var y = () => () => () => super;
        //~^ ERROR: 'super' must be followed by an argument list or member access.
        //~| ERROR: Identifier expected.
    }
}