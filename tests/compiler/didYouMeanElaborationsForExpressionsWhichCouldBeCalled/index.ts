// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/didYouMeanElaborationsForExpressionsWhichCouldBeCalled.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class Bar {
    x!: string;
}

declare function getNum(): number;

declare function foo(arg: { x: Bar, y: Date }, item: number, items?: [number, number, number]): void;

foo({
    x: Bar,
    //~^ ERROR: Property 'x' is missing.
    y: Date
    //~^ ERROR: Type 'DateConstructor' is missing the following properties from type 'Date': getVarDate, toDateString, and 40 more.
}, getNum());

foo({
    x: new Bar(),
    y: new Date()
}, getNum);
//~^ ERROR: Argument of type '() => number' is not assignable to parameter of type 'number'.

foo({
    x: new Bar(),
    y: new Date()
}, getNum(), [
    1,
    2,
    getNum
//~^ ERROR: Type '() => number' is not assignable to type 'number'.
]);
