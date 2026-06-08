declare function f(addUndefined1: any, addUndefined2: number): number;
declare function g(addUndefined: any, addDefined: number): number;
declare let total: number;


declare function foo1(x: string, b: number): void;
declare function foo2(x: any, b: number): void;
declare function foo3(x: string | undefined, b: number): void;
declare function foo4(x: string | undefined, b: number): void;
type OptionalNullableString = string | null | undefined;
declare function allowsNull(val: OptionalNullableString): void;





declare function removeUndefinedButNotFalse(x: any): undefined | false;
declare const cond: boolean;

declare function removeNothing(y: any): false | true;
