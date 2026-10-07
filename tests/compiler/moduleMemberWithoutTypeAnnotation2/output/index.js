// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/moduleMemberWithoutTypeAnnotation2.ts`, Apache-2.0 License
//@compiler-options: strict=false
//@compiler-options: target=es2015
var TypeScript = {};
(function (TypeScript) {

  var CompilerDiagnostics = {};
  (function (CompilerDiagnostics) {
  
    var diagnosticWriter = null;
    CompilerDiagnostics.diagnosticWriter = diagnosticWriter
    
    function Alert(output) {
      if (diagnosticWriter) {
        diagnosticWriter.Alert(output);
      }
      
    }
    CompilerDiagnostics.Alert = Alert;
    
  })(CompilerDiagnostics);
  TypeScript.CompilerDiagnostics = CompilerDiagnostics;
  
})(TypeScript);