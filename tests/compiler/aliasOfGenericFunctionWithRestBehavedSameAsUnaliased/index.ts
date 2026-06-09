// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/aliasOfGenericFunctionWithRestBehavedSameAsUnaliased.ts`, Apache-2.0 License

//@compiler-options: strict
//@compiler-options: target=es2015

type ExtendedMapper<HandledInputT, OutputT, ArgsT extends any[]> = (name : string, mixed : HandledInputT, ...args : ArgsT) => OutputT;
type a = ExtendedMapper<any, any, [any]>;
type b = ExtendedMapper<any, any, any[]>;
type test = a extends b ? "y" : "n"
let check: test = "y";


type ExtendedMapper1<HandledInputT, OutputT, ArgsT extends any[]> = (
    (name : string, mixed : HandledInputT, ...args : ArgsT) => OutputT
);

type a1 = ExtendedMapper1<any, any, [any]>;
type b1 = ExtendedMapper1<any, any, any[]>;
type test1 = a1 extends b1 ? "y" : "n"
let check1: test1 = "y";

type ExtendedMapper2<HandledInputT, OutputT, ArgsT extends any[]> = (
    {x:(name : string, mixed : HandledInputT, ...args : ArgsT) => OutputT}["x"]
);

type a2 = ExtendedMapper2<any, any, [any]>;
type b2 = ExtendedMapper2<any, any, any[]>;
type test2 = a2 extends b2 ? "y" : "n"
let check2: test2 = "y";

type a3 = (name: string, mixed: any, args_0: any) => any
type b3 = (name: string, mixed: any, ...args: any[]) => any

type test3 = a3 extends b3 ? "y" : "n"
let check3: test3 = "y";
