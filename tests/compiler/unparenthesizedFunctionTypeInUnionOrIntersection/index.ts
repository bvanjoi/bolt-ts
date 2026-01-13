// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/unparenthesizedFunctionTypeInUnionOrIntersection.ts`, Apache-2.0 License

type U1 = string | () => void;
//~^ ERROR: Function type notation must be parenthesized when used in a union type.
type U2 = string | (foo: number) => void
//~^ ERROR: Function type notation must be parenthesized when used in a union type.
type U3 = | () => number
//~^ ERROR: Function type notation must be parenthesized when used in a union type.
type U4 = | (foo?: number) => void;
//~^ ERROR: Function type notation must be parenthesized when used in a union type.
type U5 = string | (number: number, foo?: string) => void | number;
//~^ ERROR: Function type notation must be parenthesized when used in a union type.
type U6 =
  | string
  | (...args: any[]) => void
  | number;
//~^^ ERROR: Function type notation must be parenthesized when used in a union type.

type I1 = string & () => void;
//~^ ERROR: Function type notation must be parenthesized when used in an intersection type.
type I2 = string & (...foo: number[]) => void;
//~^ ERROR: Function type notation must be parenthesized when used in an intersection type.
type I3 = & () => boolean
//~^ ERROR: Function type notation must be parenthesized when used in an intersection type.
type I4 = & () => boolean & null;
//~^ ERROR: Function type notation must be parenthesized when used in an intersection type.
type I5 = string & (any: any, any2: any) => any & any;
//~^ ERROR: Function type notation must be parenthesized when used in an intersection type.
type I6 =
  & string
  & (foo: any) => void;
//~^ ERROR: Function type notation must be parenthesized when used in an intersection type.

type M1 = string | number & string | () => number;
//~^ ERROR: Function type notation must be parenthesized when used in a union type.
type M2 = any & string | any & () => void;
//~^ ERROR: Function type notation must be parenthesized when used in an intersection type.
type M3 = any & (foo: any) => void | () => void & any;
//~^ ERROR: Function type notation must be parenthesized when used in an intersection type.
//~| ERROR: Function type notation must be parenthesized when used in a union type.

type OK1 = string | (number);
type OK2 = string | ((number));
type OK3 = string | (()=> void);
type OK4 = string | (()=> string | number);
