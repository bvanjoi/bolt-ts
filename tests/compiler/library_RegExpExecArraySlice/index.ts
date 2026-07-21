// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/library_RegExpExecArraySlice.ts`, Apache-2.0 License

//@compiler-options: target=es2015

// RegExpExecArray.slice can have zero, one, or two arguments
var regExpExecArrayValue: RegExpExecArray;
regExpExecArrayValue.slice();
//~^ ERROR: Variable 'regExpExecArrayValue' is used before being assigned.
regExpExecArrayValue.slice(0);
//~^ ERROR: Variable 'regExpExecArrayValue' is used before being assigned.
regExpExecArrayValue.slice(0,1);
//~^ ERROR: Variable 'regExpExecArrayValue' is used before being assigned.
