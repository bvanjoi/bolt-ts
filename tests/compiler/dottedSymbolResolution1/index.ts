// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/dottedSymbolResolution1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface JQuery {
    find(selector: string): JQuery;
}

interface JQueryStatic {
    
    (selector: string): JQuery;
    (object: JQuery): JQuery;
}

class Base { foo() { } }

function each(collection: string, callback: (indexInArray: any, valueOfElement: any) => any): any;
function each(collection: JQuery, callback: (indexInArray: number, valueOfElement: Base) => any): any;
function each(collection: any, callback: (indexInArray: any, valueOfElement: any) => any): any {
    return null;
}

function _setBarAndText(): void {
    var x: JQuery, $: JQueryStatic
    each(x.find(" "), function () {
      //~^ ERROR: Variable 'x' is used before being assigned.
        var $this: JQuery = $(''),
            thisBar = $this.find(".fx-usagebars-calloutbar-this"); // bug lead to 'could not find dotted symbol' here
    } );
}