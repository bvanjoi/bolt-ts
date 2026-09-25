// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/didYouMeanSuggestionErrors.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: lib=[es5]

describe("my test suite", () => {
//~^ ERROR: Cannot find name 'describe'.
    it("should run", () => {
    //~^ ERROR: Cannot find name 'it'.
        const a = $(".thing");
        //~^ ERROR: Cannot find name '$'.
    });
});

suite("another suite", () => {
//~^ ERROR: Cannot find name 'suite'.
    test("everything else", () => {
    //~^ ERROR: Cannot find name 'test'.
        console.log(process.env);
        //~^ ERROR: Cannot find name 'process'.
        //~| ERROR: Cannot find name 'console'.
        document.createElement("div");
        //~^ ERROR: Cannot find name 'document'.
        const x = require("fs");
        //~^ ERROR: Cannot find name 'require'.
        const y = Buffer.from([]);
        //~^ ERROR: Cannot find name 'Buffer'.
        const z = module.exports;
        //~^ ERROR: Cannot find name 'module'.

        const a = new Map();
        //~^ ERROR: Cannot find name 'Map'.
        const b = new Set();
        //~^ ERROR: Cannot find name 'Set'.
        const c = new WeakMap();
        //~^ ERROR: Cannot find name 'WeakMap'.
        const d = new WeakSet();
        //~^ ERROR: Cannot find name 'WeakSet'.
        const e = Symbol();
        //~^ ERROR: Cannot find name 'Symbol'.
        const f = Promise.resolve(0);
        //~^ ERROR: Cannot find name 'Promise'.

        const i: Iterator<any> = null as any;
        //~^ ERROR: Cannot find name 'Iterator'.
        const j: AsyncIterator<any> = null as any;
        //~^ ERROR: Cannot find name 'AsyncIterator'.
        const k: Symbol = null as any;
        const l: Promise<any> = null as any;
    });
});