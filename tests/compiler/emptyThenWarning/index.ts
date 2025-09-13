// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/emptyThenWarning.ts`, Apache-2.0 License

if(1); //~ ERROR: The body of an 'if' statement cannot be the empty statement.

let x = 0;
if (true === true); {  //~ ERROR: The body of an 'if' statement cannot be the empty statement.
    x = 1;
}