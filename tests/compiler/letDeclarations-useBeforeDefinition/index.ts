// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/letDeclarations-useBeforeDefinition.ts`, Apache-2.0 License

//@compiler-options: target=ES6

{
    l1;
    //~^ ERROR: Block-scoped variable 'l1' used before its declaration.
    let l1;
}

var v1;
{
    v1;
    //~^ ERROR: Block-scoped variable 'v1' used before its declaration.
    //~| ERROR: Variable 'v1' is used before being assigned.
    let v1 = 0;
}
