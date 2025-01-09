// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/innerFunc.ts`, Apache-2.0 License

function salt() {
  function pepper() { return 5;}
  return pepper();  
}

module M {
    export function tungsten() {
        function oxygen() { return 6; };
        return oxygen();
    }
}
