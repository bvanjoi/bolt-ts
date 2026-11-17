/// <reference path="./internal.ts" preserve="true" />
namespace SomeOther.Thing {
    import Internal = My.Internal;
    export class Foo {
        private _which: Internal.WhichThing;
        //~^ ERROR: Property '_which' has no initializer and is not definitely assigned in the constructor.
        constructor() {
            Internal.getThing();
            Internal.WhichThing.A ? "foo" : "bar";
            //~^ ERROR: This condition will always return 'false'.
        }
    }
}