// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declareExternalModuleWithExportAssignedFundule.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare module "express" {

    export = express;

    function express(): express.ExpressServer;

    namespace express {

        export interface ExpressServer {

            enable(name: string): ExpressServer;

            post(path: RegExp, handler: (req: Function) => void ): void;

        }

        export class ExpressServerRequest {

        }

    }

}
