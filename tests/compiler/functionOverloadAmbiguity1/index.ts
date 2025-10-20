// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/functionOverloadAmbiguity1.ts`, Apache-2.0 License

function callb(lam: (l: number) => void );
function callb(lam: (n: string) => void );
function callb(a) { }
callb((a) => { a.length; } ); // error, chose first overload
//~^ ERROR: Property 'length' does not exist on type 'number'.

function callb2(lam: (n: string) => void );
function callb2(lam: (l: number) => void );
function callb2(a) { }
callb2((a) => { a.length; } ); // ok, chose first overload
