// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/exportDeclarationsInAmbientNamespaces.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@run-fail

declare namespace Q {
    function _try(method: Function, ...args: any[]): any;
    export { _try as try };
}

Q.try(() => { });

