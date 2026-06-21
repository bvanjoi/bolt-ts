// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/enumAssignmentCompat7.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: strictFunctionTypes

namespace first {
    export enum E { A = 1 }
}

namespace second {
    export enum E { A = 2 }
}

class Base {
    method(param: first.E) {

    }
}

class Derived extends Base {
    override method(param: second.E) {
      //~^ ERROR: Property 'method' in type 'Derived<Derived>' is not assignable to the same property in base type 'Base<Derived>'.
    }
}

function overloadingFunction(): first.E
//~^ ERROR: This overload signature is not compatible with its implementation signature.
function overloadingFunction(): second.E {
    return second.E.B
    //~^ ERROR: Property 'B' does not exist on type '{ A: E.A; }'.
}