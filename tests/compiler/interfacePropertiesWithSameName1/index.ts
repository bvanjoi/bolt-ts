// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/interfacePropertiesWithSameName1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface Mover {
    move(): void;
    getStatus(): { speed: number; };
}
interface Shaker {
    shake(): void;
    getStatus(): { frequency: number; };
}

interface MoverShaker extends Mover, Shaker {
    getStatus(): { speed: number; frequency: number; };
}
