// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/lastPropertyInLiteralWins.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface Thing {
    thunk: (str: string) => void;
}
function test(thing: Thing) {
    thing.thunk("str");
}
test({ // Should error, as last one wins, and is wrong type
    thunk: (str: string) => {},
    thunk: (num: number) => {}
    //~^ ERROR: An object literal cannot have multiple properties with the same name.
    //~| ERROR: An object literal cannot have multiple properties with the same name.
    //~| ERROR: An object literal cannot have multiple properties with the same name.
    //~| ERROR: Type '(num: number) => void' is not assignable to type '(str: string) => void'.
});

test({ // Should be OK.  Last 'thunk' is of correct type
    thunk: (num: number) => {},
    thunk: (str: string) => {}
    //~^ ERROR: An object literal cannot have multiple properties with the same name.
});
