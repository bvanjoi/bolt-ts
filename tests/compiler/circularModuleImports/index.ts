// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/circularlyReferentialInterfaceAccessNoCrash.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace M
 
{
 
    import A = B;   //~ERROR: Circular definition of import alias 'A'.
 
    import B = A;
 
}
