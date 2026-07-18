// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/globalThisCapture.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

// Add a lambda to ensure global 'this' capture is triggered
(()=>this.window);

var parts = [];

// Ensure that the generated code is correct
parts[0];
