// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/moduleMemberWithoutTypeAnnotation2.ts`, Apache-2.0 License

//@compiler-options: strict=false
//@compiler-options: target=es2015

namespace TypeScript {
    export namespace CompilerDiagnostics {

        export interface IDiagnosticWriter {
            Alert(output: string): void;
        }

        export var diagnosticWriter = null;

        export function Alert(output: string) {
            if (diagnosticWriter) {
                diagnosticWriter.Alert(output);
            }
        }
    }
}
