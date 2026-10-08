// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeUsedAsTypeLiteralIndex.ts`, Apache-2.0 License

//@compiler-options: target=esnext

type K = number | string;
type T = {
    [K]: number;  // Did you mean to use 'P in K'?
    //~^ ERROR: Cannot find name 'K'.
}

const K1 = Symbol();
type T1 = {
    [K1]: number;
}

type K2 = "x" | "y";
type T2 = {
    [K2]: number;  // Did you mean to use 'K in K2'?
    //~^ ERROR: Cannot find name 'K2'.
}

type K3 = number | string;
type T3 = {
    [K3]: number; // Did you mean to use 'K in K3'?
    //~^ ERROR: Cannot find name 'K3'.
}

type K4 = number | string;
type T4 = {
    [K4]: number;
    //~^ ERROR: Cannot find name 'K4'.
    k4: string;
}
