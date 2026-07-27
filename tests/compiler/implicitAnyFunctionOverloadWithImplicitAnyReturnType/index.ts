// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/implicitAnyFunctionOverloadWithImplicitAnyReturnType.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitAny


// this should be an error
interface IFace {
    funcOfIFace();  // error at "f"
    //~^ ERROR: 'funcOfIFace', which lacks return-type annotation, implicitly has an 'any' return type.
}

// this should not be an error
interface IFace1{
    f1(): any;
}