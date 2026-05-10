// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/nestedBlockScopedBindings9.ts`, Apache-2.0 License

//@compiler-options: target=es2015

{
    let x;
    () => x;
}

switch (1) {
    case 1:
        let y;
        () => y;
        break;
}