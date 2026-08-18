// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noImplicitSymbolToString.ts`, Apache-2.0 License

//@compiler-options: target=es2015

let symbol!: symbol;
let str = "hello ";

const templateStr = `hello ${symbol}`;
//~^ ERROR: Implicit conversion of a 'symbol' to a 'string' will fail at runtime. Consider wrapping this expression in 'String(...)'.
const appendStr = "hello " + symbol;
//~^ ERROR: The '+' operator cannot be applied to type 'symbol'.
str += symbol;
//~^ ERROR: The '+=' operator cannot be applied to type 'symbol'.

let symbolUnionNumber!: symbol | number;
let symbolUnionString!: symbol | string;

const templateStrUnion = `union with number ${symbolUnionNumber} and union with string ${symbolUnionString}`;
//~^ ERROR: Implicit conversion of a 'symbol' to a 'string' will fail at runtime. Consider wrapping this expression in 'String(...)'.
//~| ERROR: Implicit conversion of a 'symbol' to a 'string' will fail at runtime. Consider wrapping this expression in 'String(...)'.


// Fix #44462

type StringOrSymbol = string | symbol;

function getKey<S extends StringOrSymbol>(key: S) {
    return `${key} is the key`;
//~^ ERROR: Implicit conversion of a 'symbol' to a 'string' will fail at runtime. Consider wrapping this expression in 'String(...)'.
}

function getKey1<S extends symbol>(key: S) {
    let s1!: S;
    `${s1}`;
    //~^ ERROR: Implicit conversion of a 'symbol' to a 'string' will fail at runtime. Consider wrapping this expression in 'String(...)'.
    s1 + '';
    //~^ ERROR: The '+' operator cannot be applied to type 'symbol'.
    +s1;
    //~^ ERROR: The '+' operator cannot be applied to type 'symbol'.

    let s2!: S | string;
    `${s2}`;
    //~^ ERROR: Implicit conversion of a 'symbol' to a 'string' will fail at runtime. Consider wrapping this expression in 'String(...)'.
    s2 + '';
    //~^ ERROR: The '+' operator cannot be applied to type 'symbol'.
    +s2;
    //~^ ERROR: The '+' operator cannot be applied to type 'symbol'.
}

function getKey2<S extends string>(key: S) {
    let s1!: S;
    `${s1}`;
    s1 + '';
    +s1;

    let s2!: S | symbol;
    `${s2}`;
    //~^ ERROR: Implicit conversion of a 'symbol' to a 'string' will fail at runtime. Consider wrapping this expression in 'String(...)'.
    s2 + '';
    //~^ ERROR: The '+' operator cannot be applied to type 'symbol'.
    +s2;
    //~^ ERROR: The '+' operator cannot be applied to type 'symbol'.
}
