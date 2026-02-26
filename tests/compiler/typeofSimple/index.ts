// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/typeofSimple.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var v = 3;
var v2: typeof v = v;
var v3: string = v2; // Not assignment compatible
//~^ ERROR: Type 'number' is not assignable to type 'string'.

interface I<T> { x: T; }
interface J { }

declare var numberJ: typeof J; //Error, cannot reference type in typeof
//~^ ERROR: Cannot find name 'J'.
declare var numberI: I<typeof v2>;

declare var fun: () => I<number>;
numberI = fun();