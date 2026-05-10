// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/es6ImportNamedImportIdentifiersParsing.ts`, Apache-2.0 License

//@compiler-options: target=es6

import { yield } from "somemodule"; // Allowed
//~^ ERROR: Cannot find module 'somemodule' or its corresponding type declarations.
import { default } from "somemodule"; // Error - as this is keyword that is not allowed as identifier
//~^ ERROR: Cannot find module 'somemodule' or its corresponding type declarations.
import { yield as default } from "somemodule"; // error to use default as binding name
//~^ ERROR: Cannot find module 'somemodule' or its corresponding type declarations.
//~| ERROR: Duplicate identifier 'default'.
import { default as yield } from "somemodule"; // no error 
//~^ ERROR: Cannot find module 'somemodule' or its corresponding type declarations.
//~| ERROR: Duplicate identifier 'yield'.
import { default as default } from "somemodule"; // default as is ok, error of default binding name
//~^ ERROR: Cannot find module 'somemodule' or its corresponding type declarations.
//~| ERROR: Duplicate identifier 'default'.
