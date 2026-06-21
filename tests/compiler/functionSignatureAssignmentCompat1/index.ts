// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionSignatureAssignmentCompat1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface ParserFunc {
    (eventEmitter: number, buffer: string): void;
}
interface Parsers {
    raw: ParserFunc;
    readline(delimiter?: string): ParserFunc;
}
declare var parsers: Parsers;
var c: ParserFunc = parsers.raw; // ok!
var d: ParserFunc = parsers.readline; // not ok
//~^ ERROR: Type '(delimiter: string) => ParserFunc' is not assignable to type 'ParserFunc'.
var e: ParserFunc = parsers.readline(); // ok