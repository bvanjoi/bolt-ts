// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/implicitAnyDeclareMemberWithoutType.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitAny

interface IFace {
    member1;  // error at "member1"
    //~^ ERROR: Variable 'member1' implicitly has an 'any' type.
    member2: string;
    constructor(c1, c2: string, c3);  // error at "c1, c3, "constructor"
    //~^ ERROR: 'constructor', which lacks return-type annotation, implicitly has an 'any' return type.
    //~| ERROR: Parameter 'c1' implicitly has an 'any' type.
    //~| ERROR: Parameter 'c3' implicitly has an 'any' type.
    funcOfIFace(f1, f2, f3: number);   // error at "f1, f2, funcOfIFace"
    //~^ ERROR: 'funcOfIFace', which lacks return-type annotation, implicitly has an 'any' return type.
    //~| ERROR: Parameter 'f1' implicitly has an 'any' type.
    //~| ERROR: Parameter 'f2' implicitly has an 'any' type.
    new ();
    //~^ ERROR: Construct signature, which lacks return-type annotation, implicitly has an 'any' return type.
}
