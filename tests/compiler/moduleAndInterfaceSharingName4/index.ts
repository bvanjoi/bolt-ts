// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/moduleAndInterfaceSharingName4.ts`, Apache-2.0 License

declare namespace D3 {
    var x: D3.Color.Color;

    namespace Color {
        export interface Color {
            darker: Color;
        }
    }
}