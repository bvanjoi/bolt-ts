// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/moduleWithTryStatement1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: module=commonjs

interface connectModule {
    (res, req, next): void;
}
interface connectExport {
    use: (mod: connectModule) => connectExport;
    listen: (port: number) => void;
}
declare const server: {
    (): connectExport;
    test1: connectModule;
    test2(): connectModule;
};
export = server;
export = connectExport;
//~^ ERROR: Duplicate identifier 'export ='.
