// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/evalAfter0.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: allowUnreachableCode=false

(0,eval)("10"); // fine: special case for eval

declare var eva;
(0,eva)("10"); // error: no side effect left of comma (suspect of missing method name or something)
//~^ ERROR: Left side of comma operator is unused and has no side effects.