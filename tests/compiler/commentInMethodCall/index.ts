// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/commentInMethodCall.ts`, Apache-2.0 License

//@compiler-options: target=es2015

//commment here
var s: string[];
s.map(// do something   //~ERROR: Variable 's' is used before being assigned.
    function () { });
