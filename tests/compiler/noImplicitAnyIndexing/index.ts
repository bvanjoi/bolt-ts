// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noImplicitAnyIndexing.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitAny

enum MyEmusEnum {
    emu
}

// Should be okay; should be a string.
var strRepresentation1 = MyEmusEnum[0]

// Should be okay; should be a string.
var strRepresentation2 = MyEmusEnum[MyEmusEnum.emu]

// Should be implicit 'any' ; property access fails, no string indexer.
var strRepresentation3 = MyEmusEnum["monehh"];
//~^ ERROR: Element implicitly has an 'any' type because index expression is not of type 'number'.

// Should be okay; should be a MyEmusEnum
var strRepresentation4 = MyEmusEnum["emu"];


// Should report an implicit 'any'.
var x = {}["hi"];
//~^ ERROR: Property '"hi"' does not exist on type '{ }'.

// Should report an implicit 'any'.
var y = {}[10];
//~^ ERROR: Property '10' does not exist on type '{ }'.


var hi: any = "hi";

var emptyObj = {};

// Should report an implicit 'any'.
var z1 = emptyObj[hi];
//~^ ERROR: Element implicitly has an 'any' type because expression of type 'any' can't be used to index type '{ }'.
var z2 = (<any>emptyObj)[hi];

interface MyMap<T> {
    [key: string]: T;
}

var m: MyMap<number> = {
    "0": 0,
    "1": 1,
    "2": 2,
    "Okay that's enough for today.": NaN
};

var mResult1 = m[MyEmusEnum.emu];
var mResult2 = m[MyEmusEnum[MyEmusEnum.emu]];
var mResult3 = m[hi];

