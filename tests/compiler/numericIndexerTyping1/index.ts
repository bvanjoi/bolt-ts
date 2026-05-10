// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/numericIndexerTyping1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface I {
    [x: string]: Date;
}

interface I2 extends I {
}

declare var i: I;
var r: string = i[1]; // error: numeric indexer returns the type of the string indexer
//~^ ERROR: Type 'Date' is not assignable to type 'string'.

declare var i2: I2;
var r2: string = i2[1]; // error: numeric indexer returns the type of the string indexer
//~^ ERROR: Type 'Date' is not assignable to type 'string'.
