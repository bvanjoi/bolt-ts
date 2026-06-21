// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declaredExternalModule.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

declare module 'connect' {

    interface connectModule {

        (res, req, next): void;

    }

    interface connectExport {

        use: (mod: connectModule) => connectExport;

        listen: (port: number) => void;

    }

    var server: {

        (): connectExport;

        test1: connectModule;   // No error

        test2(): connectModule; // ERROR: Return type of method from exported interface has or is using private type ''connect'.connectModule'.

    };
}
