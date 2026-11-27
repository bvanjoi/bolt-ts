// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionsWithModifiersInBlocks1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
{
   declare function f() { }
   //~^ ERROR: Duplicate function implementation.
   //~| ERROR: An implementation cannot be declared in ambient contexts.
   export function f() { }
   //~^ ERROR: An export declaration can only be used at the top level of a namespace or module.
   //~| ERROR: Duplicate function implementation.
   declare export function f() { }
   //~^ ERROR: 'export' modifier must precede 'declare' modifier.
   //~| ERROR: An export declaration can only be used at the top level of a namespace or module.
   //~| ERROR: Duplicate function implementation.
   //~| ERROR: An implementation cannot be declared in ambient contexts.
}