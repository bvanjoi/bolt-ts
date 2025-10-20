// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/enumMapBackIntoItself.ts`, Apache-2.0 License

enum TShirtSize {
   Small,
   Medium,
   Large
}
var mySize = TShirtSize.Large;
var test = TShirtSize[mySize];
// specifically checking output here, bug was that test used to be undefined at runtime
test + ''

var mySize1: TShirtSize = TShirtSize.Large;
var test1: string = TShirtSize[mySize2]

var mySize2: TShirtSize = TShirtSize.Large;
var test2: number = TShirtSize[mySize2]
//~^ ERROR: Type 'string' is not assignable to type 'number'.


var mySize3: number = TShirtSize.Large;
var test3: number = TShirtSize[mySize3]
//~^ ERROR: Type 'string' is not assignable to type 'number'.