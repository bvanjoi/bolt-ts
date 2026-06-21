// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declarationEmitDestructuringParameterProperties2.ts`, Apache-2.0 License

//@compiler-options: target=es2024
//@compiler-options: module=commonjs
//@compiler-options: declaration

class C1 {
    constructor(public [x, y, z]: string[]) {
      //~^ ERROR: A parameter property may not be declared using a binding pattern.
    }
}

type TupleType1 =[string, number, boolean];
class C2 {
    constructor(public [x, y, z]: TupleType1) {
      //~^ ERROR: A parameter property may not be declared using a binding pattern.
    }
}

type ObjType1 = { x: number; y: string; z: boolean }
class C3 {
    constructor(public { x, y, z }: ObjType1) {
      //~^ ERROR: A parameter property may not be declared using a binding pattern.
    }
}