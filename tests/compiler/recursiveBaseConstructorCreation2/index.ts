// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/recursiveBaseConstructorCreation2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@run-fail

declare class base
{
}
declare class abc extends base
{
   foo: xyz;
}
declare class xyz extends abc
{
}
 
var bar = new xyz(); // Error: Invalid 'new' expression.
 
