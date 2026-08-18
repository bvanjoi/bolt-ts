// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/controlFlowInitializedDestructuringVariables.ts`, Apache-2.0 License
//@compiler-options: strict

var {a = '0', b = +a} = obj;
{
  function m(a) {
    var a1 = a;
  }
  111000n;
}