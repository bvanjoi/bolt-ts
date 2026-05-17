// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionExportsRequireAndAmbientFunctionInGlobalFile.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare function exports(): number;
declare function require(): string;
declare namespace m3 {
    function exports(): string[];
    function require(): number[];
}
namespace m4 {
    export declare function exports(): string;
    export declare function require(): string;
  var a = 10;
}