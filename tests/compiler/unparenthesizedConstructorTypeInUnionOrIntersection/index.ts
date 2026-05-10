// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unparenthesizedConstructorTypeInUnionOrIntersection.ts`, Apache-2.0 License

//@compiler-options: target=es2015

type U1 = string | new () => void;
//~^ ERROR: Constructor Function type notation must be parenthesized when used in a union type.
type U2 = string | new (foo: number) => void
//~^ ERROR: Constructor Function type notation must be parenthesized when used in a union type.
type U3 = | new () => number
//~^ ERROR: Constructor Function type notation must be parenthesized when used in a union type.
type U4 = | new (foo?: number) => void;
//~^ ERROR: Constructor Function type notation must be parenthesized when used in a union type.
type U5 = string | new (number: number, foo?: string) => void | number;
//~^ ERROR: Constructor Function type notation must be parenthesized when used in a union type.
type U6 =
  | string
  | new (...args: any[]) => void //~ERROR: Constructor Function type notation must be parenthesized when used in a union type.
  | number;

type I1 = string & new () => void;
//~^ ERROR: Constructor Function type notation must be parenthesized when used in an intersection type.
type I2 = string & new (...foo: number[]) => void;
//~^ ERROR: Constructor Function type notation must be parenthesized when used in an intersection type.
type I3 = & new () => boolean
//~^ ERROR: Constructor Function type notation must be parenthesized when used in an intersection type.
type I4 = & new () => boolean & null;
//~^ ERROR: Constructor Function type notation must be parenthesized when used in an intersection type.
type I5 = string & new (any: any, any2: any) => any & any;
//~^ ERROR: Constructor Function type notation must be parenthesized when used in an intersection type.
type I6 =
  & string
  & new (foo: any) => void;
//~^ ERROR: Constructor Function type notation must be parenthesized when used in an intersection type.

type M1 = string | number & string | new () => number;
//~^ ERROR: Constructor Function type notation must be parenthesized when used in a union type.
type M2 = any & string | any & new () => void;
//~^ ERROR: Constructor Function type notation must be parenthesized when used in an intersection type.
type M3 = any & new (foo: any) => void | new () => void & any;
//~^ ERROR: Constructor Function type notation must be parenthesized when used in an intersection type.
//~| ERROR: Constructor Function type notation must be parenthesized when used in a union type.

type OK1 = string | (new ()=> void);
type OK2 = string | (new ()=> string | number);
