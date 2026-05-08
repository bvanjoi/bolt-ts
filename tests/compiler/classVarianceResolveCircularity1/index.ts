// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/classVarianceResolveCircularity1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

class Bar<T> {
    num!: number;
    Value = callme(this).num;
    Field: number = callme(this).num;
}
declare function callme(x: Bar<any>): Bar<any>;
declare function callme(x: object): string;