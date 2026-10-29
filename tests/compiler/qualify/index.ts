// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/qualify.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

namespace M {
    export var m=0;
    export namespace N {
        export var n=1;
    }
}

namespace M {
    export namespace N {
        var y=m;
        var x=n+y;
    }
}


namespace T {
    export interface I {
        p;
    }
    export namespace U {
        var z:I=3;
        //~^ ERROR: Type 'number' is not assignable to type 'T.I'.
        export interface I2 {
            q;
        }
    }
}

namespace Peer {
    export namespace U2 {
        var z:T.U.I2=3;
        //~^ ERROR: Type 'number' is not assignable to type 'T.U.I2'.
    }
}

namespace Everest {
    export namespace K1 {
        export interface I3 {
            zeep;
        }
    }
    export namespace K2 {
        export interface I4 {
            z;
        }
        var v1:I4 = undefined as any;
        var v2:K1.I3=v1;
        //~^ ERROR: Property 'zeep' is missing.
        var v3:K1.I3[]=v1;
        //~^ ERROR: Type 'Everest.K2.I4' is missing the following properties from type 'Everest.K1.I3[]': length, pop, and 27 more.
        var v4:()=>K1.I3=v1;
        //~^ ERROR: Type 'Everest.K2.I4' is not assignable to type '() => Everest.K1.I3'.
        var v5:(k:K1.I3)=>void=v1;
        //~^ ERROR: Type 'Everest.K2.I4' is not assignable to type '(k: Everest.K1.I3) => void'.
        var v6:{k:K1.I3;}=v1;
        //~^ ERROR: Property 'k' is missing.
    }
}

interface I {
    k;
}

var y:I = undefined as any;
var x:T.I=y;
//~^ ERROR: Property 'p' is missing.