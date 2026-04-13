// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/interfacePropertiesWithSameName2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface Mover {
    move(): void;
    getStatus(): { speed: number; };
}
interface Shaker {
    shake(): void;
    getStatus(): { frequency: number; };
}

interface MoverShaker extends Mover, Shaker { //~ERROR: Interface 'MoverShaker' cannot simultaneously extend types 'Mover' and 'Shaker'.

}

// Inside a module
declare namespace MoversAndShakers {
    export class Mover {
        move(): void;
        getStatus(): { speed: number; };
    }
    export interface Shaker {
        shake(): void;
        getStatus(): { frequency: number; };
    }
}

interface MoverShaker2 extends MoversAndShakers.Mover, MoversAndShakers.Shaker { } // error
//~^ ERROR: Interface 'MoverShaker2' cannot simultaneously extend types 'Mover' and 'Shaker'.

interface MoverShaker3 extends MoversAndShakers.Mover, MoversAndShakers.Shaker {
    getStatus(): { speed: number; frequency: number; }; // ok because this getStatus overrides the conflicting ones above
}