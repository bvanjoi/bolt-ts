// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/omittedExpressionForOfLoop.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

for (const [,] of doesNotExist) { //~ERROR: Cannot find name 'doesNotExist'.
}

for (const [,] of undefined) {    //~ERROR: The value 'undefined' cannot be used here.
}

for (const [,] of []) {     //~ERROR: Type 'never' must have a '[Symbol.iterator]()' method that returns an iterator.
}

for (const [] of []) {     //~ERROR: Type 'never' must have a '[Symbol.iterator]()' method that returns an iterator.
}