// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/weakType.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface Settings {
    timeout?: number;
    onError?(): void;
}

function getDefaultSettings() {
    return { timeout: 1000 };
}
interface CtorOnly {
    new(s: string): { timeout: 1000 }
}

function doSomething(settings: Settings) { /* ... */ }
// forgot to call `getDefaultSettings`
doSomething(getDefaultSettings);
//~^ ERROR: Value of type '() => { timeout: number; }' has no properties in common with type 'Settings'. Did you mean to call it?
doSomething(() => ({ timeout: 1000 }));
//~^ ERROR: Value of type '() => { timeout: number; }' has no properties in common with type 'Settings'. Did you mean to call it?
doSomething(null as CtorOnly);
//~^ ERROR: Conversion of type 'null' to type 'CtorOnly' may be a mistake because neither type sufficiently overlaps with the other. If this was intentional, convert the expression to 'unknown' first.
//~| ERROR: Value of type 'CtorOnly' has no properties in common with type 'Settings'. Did you mean to call it?
doSomething(12);
//~^ ERROR: Type '12' has no properties in common with type 'Settings'.
doSomething('completely wrong');
//~^ ERROR: Type '"completely wrong"' has no properties in common with type 'Settings'.
doSomething(false);
//~^ ERROR: Type 'false' has no properties in common with type 'Settings'.

// this is an oddly popular way of defining settings
// this example is from services/textChanges.ts
type ConfigurableStart = { useStart?: boolean }
type ConfigurableEnd = { useEnd?: boolean }
type ConfigurableStartEnd = ConfigurableStart & ConfigurableEnd
interface InsertOptions {
    prefix?: string
    suffix?: string
}
type ChangeOptions = ConfigurableStartEnd & InsertOptions;

function del(options: ConfigurableStartEnd = {},
             error: { error?: number } = {}) {
    let changes: ChangeOptions[] = [];
    changes.push(options);
    changes.push(error);
    //~^ ERROR: Type '{ error: undefined | number; }' has no properties in common with type 'ChangeOptions'.
}

class K {
    constructor(s: string) { }
}
// Ctor isn't a weak type because it has a construct signature
interface Ctor {
    new (s: string): K
    n?: number
}
let ctor: Ctor = K

type Spoiler = { nope?: string }
type Weak = {
    a?: number
    properties?: {
        b?: number
    }
}
declare let propertiesWrong: {
    properties: {
        wrong: string
    }
}
let weak: Weak & Spoiler = propertiesWrong
//~^ ERROR: Type '{ wrong: string; }' has no properties in common with type '{ b: undefined | number; }'
