declare function f(addUndefined1: string, addUndefined2?: number): number;
declare function g(addUndefined: string, addDefined: number): number;
declare var total: number;

declare function foo1(x: string, b: number): void;
declare function foo2(x: string, b: number): void;
declare function foo3(x: string | undefined, b: number): void;
declare function foo4(x: string | undefined, b: number): void;
type OptionalNullableString = string | null | undefined;
declare function allowsNull(val: OptionalNullableString): void;





declare function removeUndefinedButNotFalse(x: boolean): undefined | false;
declare var cond: boolean;
declare function removeNothing(y: undefined | false | true): false | true;
