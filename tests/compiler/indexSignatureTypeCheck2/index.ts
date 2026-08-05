// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/indexSignatureTypeCheck2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class IPropertySet {
    [index: string]: any
}

var ps: IPropertySet = null;
//~^ ERROR: Type 'null' is not assignable to type 'IPropertySet'.
var index: any = "hello";
ps[index] = 12;

interface indexErrors {
    [p2?: string];
    //~^ ERROR: An index signature parameter cannot have a question mark.
    //~| ERROR: An index signature must have a type annotation.
    //~| ERROR: Duplicate index signature for type 'string'.
    [...p3: any[]];
    //~^ ERROR: An index signature cannot have a rest parameter.
    //~| ERROR: An index signature must have a type annotation.
    //~| ERROR: An index signature parameter type must be 'string', 'number', 'symbol', or a template literal type.
    [p4: string, p5?: string];
    //~^ ERROR: An index signature cannot have a trailing comma.
    //~| ERROR: Expected ']'.
    //~| ERROR: An index signature must have a type annotation.
    //~| ERROR: Property or signature expected.
    //~| ERROR: Property or signature expected.
    //~| ERROR: Duplicate index signature for type 'string'.
    [p6: string, ...p7: any[]];
    //~^ ERROR: An index signature cannot have a trailing comma.
    //~| ERROR: Expected ']'.
    //~| ERROR: An index signature must have a type annotation.
    //~| ERROR: Property or signature expected.
    //~| ERROR: Property or signature expected.
    //~| ERROR: Property or signature expected.
    //~| ERROR: Duplicate index signature for type 'string'.
}