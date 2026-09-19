declare function simpleFunc(): string;
declare var simpleFuncVar: () => string;
declare function anotherFuncNoReturn(): void;
declare var anotherFuncNoReturnVar: () => void;
declare function withReturn(): string;
declare var withReturnVar: () => string;
declare function withParams(a: string): string;
declare var withparamsVar: (a: string) => string;
declare function withMultiParams(a: number, b: any, c: Object): number;
declare var withMultiParamsVar: (a: number, b: any, c: Object) => number;
declare function withOptionalParams(a?: string): void;
declare var withOptionalParamsVar: (a: string) => void;
declare function withInitializedParams(a: string, b0: any, b?: number, c?: string): void;
declare var withInitializedParamsVar: (a: string, b0: any, b: number, c: string) => void;
declare function withOptionalInitializedParams(a: string, c?: string): void;
declare var withOptionalInitializedParamsVar: (a: string, c: string) => void;
declare function withRestParams(a: string, ...myRestParameter: number[]): number[];
declare var withRestParamsVar: (a: string, ...myRestParameter: number[]) => number[];
declare function overload1(n: number): string;
declare function overload1(s: string): string;
declare function overload1(ns: any): any;
declare var withOverloadSignature: (n: number) => string;
declare function f(n: () => void): void;
declare namespace m2 {
  function foo(n: () => void): void;
}

declare function fooAmbient(n: number): string;
declare function overloadAmbient(n: number): string;
declare function overloadAmbient(s: string): string;
declare var f2: () => string;
