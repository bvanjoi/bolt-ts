// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/crashIntypeCheckObjectCreationExpression.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=amd

export class BuildWorkspaceService {
    public injectRequestService<P0, P1, P2>(service: P0) {
        this.injectBuildService<number>(new X(service));
        //~^ ERROR: Cannot find name 'X'.
    }
    public injectBuildService<P0>(service: P0) {
    }
}
