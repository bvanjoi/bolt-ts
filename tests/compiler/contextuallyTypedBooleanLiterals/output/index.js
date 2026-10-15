// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contextuallyTypedBooleanLiterals.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: declaration
//@run-fail
var bn1 = box(0);// Box<number>

var bn2 = box(0);// Ok

var bb1 = box(false);// Box<boolean>

var bb2 = box(false);// Error, box<false> not assignable to Box<boolean>

var x = observable(false);