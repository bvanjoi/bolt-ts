// From `github.com/microsoft/TypeScript/blob/v5.8.3/tests/cases/compiler/continueLabel.ts`, Apache-2.0 License

label1: for(var i = 0; i < 1; i++) {
    continue label1;
}