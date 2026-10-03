// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedIdentifiersConsolidated1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

function greeter(person: string) {
  //~^ ERROR: 'person' is declared but its value is never read.
    var unused = 20;
  //~^ ERROR: 'unused' is declared but its value is never read.
}

class Dummy<usedtypeparameter, unusedtypeparameter> {
  //~^ ERROR: 'unusedtypeparameter' is declared but its value is never read.
    private unusedprivatevariable: string;
  //~^ ERROR: 'unusedprivatevariable' is declared but its value is never read.
  //~| ERROR: Property 'unusedprivatevariable' has no initializer and is not definitely assigned in the constructor.
    private greeting: string;
  //~^ ERROR: 'greeting' is declared but its value is never read.
    public unusedpublicvariable: string;
  //~^ ERROR: Property 'unusedpublicvariable' has no initializer and is not definitely assigned in the constructor.
    public typedvariable: usedtypeparameter;
  //~^ ERROR: Property 'typedvariable' has no initializer and is not definitely assigned in the constructor.

    constructor(message: string) {
      //~^ ERROR: 'message' is declared but its value is never read.
        var unused2 = 22;
        //~^ ERROR: 'unused2' is declared but its value is never read.
        this.greeting = "Dummy Message";
    }

    public greeter(person: string) {
      //~^ ERROR: 'person' is declared but its value is never read.
        var unused = 20;
        //~^ ERROR: 'unused' is declared but its value is never read.
        this.usedPrivateFunction();
    }

    private usedPrivateFunction() {
    }

    private unUsedPrivateFunction() {
      //~^ ERROR: 'unUsedPrivateFunction' is declared but its value is never read.
    }
}

var user = "Jane User";
var user2 = "Jane2 User2";

namespace Validation {
    export interface StringValidator {
        isAcceptable(s: string): boolean;
    }

    const lettersRegexp = /^[A-Za-z]+$/;
    const numberRegexp = /^[0-9]+$/;
    //~^ ERROR: 'numberRegexp' is declared but its value is never read.

    export class LettersOnlyValidator implements StringValidator {
        isAcceptable(s2: string) {
            return lettersRegexp.test(s2);
        }

        private unUsedPrivateFunction() {
          //~^ ERROR: 'unUsedPrivateFunction' is declared but its value is never read.
        }
    }

    export class ZipCodeValidator implements StringValidator {
        isAcceptable(s3: string) {
            return s3.length === 5;
        }
    }

    interface usedLocallyInterface {
    }

    interface usedLocallyInterface2 {
      //~^ ERROR: 'usedLocallyInterface2' is declared but never used.
        someFunction(s1: string): void;
    }

    export interface exportedInterface {
    }

    class dummy implements usedLocallyInterface {
      //~^ ERROR: 'dummy' is declared but never used.
    }

    interface unusedInterface {
      //~^ ERROR: 'unusedInterface' is declared but never used.
    }
}


namespace Greeter {
    class class1 {
    }

    export class class2 extends class1 {
    }

    class class3 {
      //~^ ERROR: 'class3' is declared but never used.
    }

    export class class4 {
    }

    interface interface1 {
    }

    export interface interface2 extends interface1 {
    }

    interface interface3 {
    }

    export interface interface4 {
    }

    export let a: interface3;

    interface interface5 {
      //~^ ERROR: 'interface5' is declared but never used.
    }
}
