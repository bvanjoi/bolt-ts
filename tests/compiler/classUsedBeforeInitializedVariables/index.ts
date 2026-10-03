// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/classUsedBeforeInitializedVariables.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

class Test {
    p1 = 0;
    p2 = this.p1;
    p3 = this.p4;
    //~^ ERROR: Property 'p4' is used before its initialization.
    p4 = 0;
    p5?: number;

    p6?: string;
    p7 = {
        hello: (this.p6 = "string"),
    };

    directlyAssigned: any = this.directlyAssigned;
    //~^ ERROR: Property 'directlyAssigned' is used before its initialization.

    withinArrowFunction: any = () => this.withinArrowFunction;

    withinFunction: any = function () {
        return this.withinFunction;
        //~^ ERROR: 'this' implicitly has type 'any' because it does not have a type annotation.
    };

    withinObjectLiteral: any = {
        [this.withinObjectLiteral]: true,
        //~^ ERROR: Property 'withinObjectLiteral' is used before its initialization.
        //~| ERROR: Property 'withinObjectLiteral' is used before its initialization.
        //~| ERROR: Property 'withinObjectLiteral' is used before its initialization.
        //~| ERROR: Property 'withinObjectLiteral' is used before its initialization.
    };

    withinObjectLiteralGetterName: any = {
        get [this.withinObjectLiteralGetterName]() {
          //~^ ERROR: Property 'withinObjectLiteralGetterName' is used before its initialization.
          //~| ERROR: Property 'withinObjectLiteralGetterName' is used before its initialization.
            return true;
        }
    };

    withinObjectLiteralSetterName: any = {
        set [this.withinObjectLiteralSetterName](_: any) {}
          //~^ ERROR: Property 'withinObjectLiteralSetterName' is used before its initialization.
          //~| ERROR: Property 'withinObjectLiteralSetterName' is used before its initialization.
    };

    withinClassDeclarationExtension: any = (class extends this.withinClassDeclarationExtension { });
      //~^ ERROR: Property 'withinClassDeclarationExtension' is used before its initialization.

    fromOptional = this.p5;

    // These error cases are ignored (not checked by control flow analysis)

    assignedByArrowFunction: any = (() => this.assignedByFunction)();

    assignedByFunction: any = (function () {
        return this.assignedByFunction;
      //~^ ERROR: 'this' implicitly has type 'any' because it does not have a type annotation.
    })();
}
