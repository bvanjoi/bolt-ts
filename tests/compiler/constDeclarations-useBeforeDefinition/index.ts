// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/constDeclarations-useBeforeDefinition.ts`, Apache-2.0 License

//@compiler-options: target=es6
//@compiler-options: strict=false

{
    c1; //~ERROR: Block-scoped variable 'c1' used before its declaration.
    const c1 = 0;
}

var v1;
{
    v1;  //~ERROR: Block-scoped variable 'v1' used before its declaration.
    const v1 = 0;
}
