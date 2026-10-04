// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/superInLambdas.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

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

        // super call in a constructor
        super.sayHello();

        // super call in a lambda in a constructor 
        var x = () => super.sayHello();
    }
    sayHello(): void {
        // super call in a method
        super.sayHello();

        // super call in a lambda in a method
       var x = () => super.sayHello();
    }
}
class RegisteredUser2 extends User {
    name: string = "Joe";
    constructor() {
        super();

        // super call in a nested lambda in a constructor 
        var x = () => () => () => super.sayHello();
    }
    sayHello(): void {
        // super call in a nested lambda in a method
        var x = () => () => () => super.sayHello();
    }
}

class RegisteredUser3 extends User {
    name: string = "Sam";
    constructor() {
        super();

        // super property in a nested lambda in a constructor 
        var superName = () => () => () => super.name;
        //~[target=ES5]^ ERROR: Only public and protected methods of the base class are accessible via the 'super' keyword.
        //~[target=ES2015]^^ ERROR: Class field 'name' defined by the parent class is not accessible in the child class via super.
    }
    sayHello(): void {
        // super property in a nested lambda in a method
        var superName = () => () => () => super.name;
        //~[target=ES5]^ ERROR: Only public and protected methods of the base class are accessible via the 'super' keyword.
        //~[target=ES2015]^^ ERROR: Class field 'name' defined by the parent class is not accessible in the child class via super.
    }
}

class RegisteredUser4 extends User {
    name: string = "Mark";
    constructor() {
        super();

        // super in a nested lambda in a constructor 
        var x = () => () => super;
        //~^ ERROR: 'super' must be followed by an argument list or member access.
        //~| ERROR: Identifier expected.
    }
    sayHello(): void {
        // super in a nested lambda in a method
        var x = () => () => super;
        //~^ ERROR: 'super' must be followed by an argument list or member access.
        //~| ERROR: Identifier expected.
    }
}