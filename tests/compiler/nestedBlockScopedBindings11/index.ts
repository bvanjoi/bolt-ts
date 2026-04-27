// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/nestedBlockScopedBindings11.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var x;
{
    let x;
    () => x;
}

var y;
switch (1) {
    case 1:
        let y;
        () => y;
        break;
}