// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeAliasDeclarationEmit3.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function f1(): void {
    for (let i = 0; i < 1; i++)
        type foo = [];  //~ ERROR: 'type' declarations can only be declared inside a block.
        console.log('f1');
}

function f2(): void {
    while (true)
        type foo = [];  //~ ERROR: 'type' declarations can only be declared inside a block.
        console.log('f2');
}

function f3(): void {
    if (true)
        type foo = [];  //~ ERROR: 'type' declarations can only be declared inside a block.
        console.log('f3');
}
