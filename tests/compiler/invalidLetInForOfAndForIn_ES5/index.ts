// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/invalidLetInForOfAndForIn_ES5.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@skip-message-match

// This should be an error
// More details: http://www.ecma-international.org/ecma-262/6.0/#sec-iteration-statements

var let = 10;
for (let of [1,2,3]) {}

for (let in [1,2,3]) {}
 

