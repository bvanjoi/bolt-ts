// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/genericNumberIndex.ts`, Apache-2.0 License

type X<I extends number> = ['a'][I];

type Y<I extends string> = ['a'][I];
//~^ ERROR: Type 'I' cannot be used to index type '["a"]'.

type Z<G extends string> = ['a'][G];
//~^ ERROR: Type 'G' cannot be used to index type '["a"]'.

type F<T> = 0 extends 0 ? T['123'] : T['456'];
//~^ ERROR: Type '"123"' cannot be used to index type 'T'.
//~| ERROR: Type '"456"' cannot be used to index type 'T'.