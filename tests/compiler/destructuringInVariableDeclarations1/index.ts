// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/destructuringInVariableDeclarations1.ts`, Apache-2.0 License

//@compiler-options: target=es6
//@compiler-options: module=commonjs

export let { toString } = 1;
{
    let { toFixed } = 1;
}
