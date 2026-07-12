// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typecheckIfCondition.ts`, Apache-2.0 License

//@compiler-options: target=es2015

// both uses of module should be an undefined symbol
function myWrapper()
{
    if (!module.exports) module.exports = "";
    //~^ ERROR: Cannot find name 'module'.
    //~| ERROR: Cannot find name 'module'.
    var x = null; // don't want to baseline output
}
