// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/strictModeWordInImportDeclaration.ts`, Apache-2.0 License

//@compiler-options: target=ES6

"use strict"
import * as package from "./1"
//~^ ERROR: Identifier expected. 'package' is a reserved word in strict mode.
//~| ERROR: Cannot find module './1' or its corresponding type declarations.
import {foo as private} from "./1"
//~^ ERROR: Identifier expected. 'private' is a reserved word in strict mode.
//~| ERROR: Cannot find module './1' or its corresponding type declarations.
import public from "./1"
//~^ ERROR: Identifier expected. 'public' is a reserved word in strict mode.
//~| ERROR: Cannot find module './1' or its corresponding type declarations.

namespace d {}
import protected = d;
//~^ ERROR: Identifier expected. 'protected' is a reserved word in strict mode.
import {type as as b} from './1';
//~^ ERROR: Cannot find module './1' or its corresponding type declarations.

export {d as package}