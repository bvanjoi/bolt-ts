// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/exportDefaultVariable.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare var io: any;

declare module 'module' {
    export default io;
}
