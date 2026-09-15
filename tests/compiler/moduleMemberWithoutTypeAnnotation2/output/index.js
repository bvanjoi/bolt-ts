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