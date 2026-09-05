// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/expandoFunctionContextualTypesNoValue.ts`, Apache-2.0 License

//@compiler-options: target=es2015

import Foo from "blah";
//~^ ERROR: Cannot find module 'blah' or its corresponding type declarations.

export function Foo() { }

Foo.bar = () => { };


function Foo2() { }
Foo2.bar = () => { };