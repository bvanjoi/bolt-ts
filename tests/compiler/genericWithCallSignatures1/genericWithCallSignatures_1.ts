///<reference path="genericWithCallSignatures_0.ts"/>
class MyClass {
    public callableThing: CallableExtention<string>;
    //~^ ERROR: Property 'callableThing' has no initializer and is not definitely assigned in the constructor.

    public myMethod() {
        var x = <string> this.callableThing();
    }
}
