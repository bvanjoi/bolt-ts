// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/nestedBlockScopedBindings12.ts`, Apache-2.0 License

var x;
{
    let x;
    x = 1;
}

var y;
switch (1) {
    case 1:
        let y;
        y = 1;
        break;
}