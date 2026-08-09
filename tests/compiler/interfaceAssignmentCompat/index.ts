// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/interfaceAssignmentCompat.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace M {
    export enum Color {
        Green,
        Blue,
        Brown,
    }

    export interface IEye {
        color:number;
    }

    export interface IFrenchEye {
        coleur:number;
    }

    export function CompareEyes(a:IEye,b:IEye):number {
        return a.color-b.color;
    }

    export function CompareYeux(a:IFrenchEye,b:IFrenchEye):number {
        return a.coleur-b.coleur;
    }

    export function test() {
        var x:IEye[]= [];
        var result="";
    
        x[0]={ color:Color.Brown };
        x[1]={ color:Color.Blue };
        x[2]={ color:Color.Green };

        x=x.sort(CompareYeux); // parameter mismatch
        //~^ ERROR: Argument of type '(a: M.IFrenchEye, b: M.IFrenchEye) => number' is not assignable to parameter of type 'undefined | ((a: M.IEye, b: M.IEye) => number)'.
        // type of z inferred from specialized array type
        var z=x.sort(CompareEyes); // ok

        for (var i=0,len=z.length;i<len;i++) {
            result+=((Color._map[z[i].color])+"\r\n");
            //~^ ERROR: Property '_map' does not exist on type '{ Green: Color.Green; Blue: Color.Blue; Brown: Color.Brown; }'.
        }

        var eeks:IFrenchEye[] = [];
        for (var j=z.length=1;j>=0;j--) {
            eeks[j]=z[j];  // nope: element assignment
            //~^ ERROR: Property 'coleur' is missing.
        }
        eeks=z; // nope: array assignment
        //~^ ERROR: Property 'coleur' is missing.
        //~| ERROR: Property 'coleur' is missing.
        return result;
    }
}

M.test();


