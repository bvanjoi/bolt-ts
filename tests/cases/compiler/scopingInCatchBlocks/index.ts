// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/scopingInCatchBlocks.ts`, Apache-2.0 License

try { } catch(ex1) { 
	throw ex1;
}

try { } catch(ex1) { } // should not error

try { } catch(ex1) { } // should not error

var x = ex1; // should error
//~^ ERROR: Cannot find name 'ex1'.
