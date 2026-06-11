// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionsMissingReturnStatementsAndExpressionsStrictNullChecks.ts`, Apache-2.0 License

//@compiler-options: target=es2018
//@compiler-options: strictNullChecks

function f10(): undefined {
    // Ok, return type allows implicit return of undefined
}

function f11(): undefined | number {
    // Error, return type isn't just undefined
    //~^^ ERROR: A function whose declared type is neither 'undefined', 'void', nor 'any' must return a value.
}

function f12(): number {
    // Error, return type doesn't include undefined
    //~^^ ERROR: A function whose declared type is neither 'undefined', 'void', nor 'any' must return a value.
}

const f20: () => undefined = () => {
    // Ok, contextual type for implicit return is undefined
}

const f21: () => undefined | number = () => {
    // Ok, contextual type for implicit return contains undefined
}

const f22: () => number = () => {
    // Error, regular void function because contextual type for implicit return isn't just undefined
    //~^^ ERROR: Type '() => void' is not assignable to type '() => number'.
}

async function f30(): Promise<undefined> {
    // Ok, return type allows implicit return of undefined
}

async function f31(): Promise<undefined | number> {
    // Error, return type isn't just undefined
    //~^^ ERROR: A function whose declared type is neither 'undefined', 'void', nor 'any' must return a value.
}

async function f32(): Promise<number> {
    // Error, return type doesn't include undefined
    //~^^ ERROR: A function whose declared type is neither 'undefined', 'void', nor 'any' must return a value.
}

// Examples from #36288

declare function f(a: () => undefined): void;

f(() => { });

f((): undefined => { });

const g1: () => undefined = () => { };

const g2 = (): undefined => { };

function h1() {
}

f(h1);  // Error
//~^ ERROR: Argument of type '() => void' is not assignable to parameter of type '() => undefined'.

function h2(): undefined {
}

f(h2);

// https://github.com/microsoft/TypeScript/issues/57840

type FN = () => Promise<undefined> | undefined;

const fn1: FN = () => {
    return;
};

const fn2: FN = async () => {
    return;
};

const fn3: FN = () => {};

const fn4: FN = async () => {};
