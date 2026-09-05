// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/extension.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

interface I {
    x;
}

interface I {
    y;
}

declare namespace M {
    export class C {
        public p:number;
    }
}

declare namespace M {
    export extension class C {
      //~^ ERROR: Declaration or statement expected.
      //~| ERROR: Unexpected keyword or identifier.
      //~| ERROR: Duplicate identifier 'C'.
      //~| ERROR: Cannot find name 'extension'.
        public pe:string;
    }
}

var c=new M.C();
c.pe;
//~^ ERROR: Property 'pe' does not exist on type 'M.C'.
c.p;
declare var i:I;
i.x;
i.y;

