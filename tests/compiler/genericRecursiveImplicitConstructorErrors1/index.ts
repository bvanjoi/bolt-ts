// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericRecursiveImplicitConstructorErrors1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=amd

export declare namespace TypeScript {
  class PullSymbol { }
  class PullSignatureSymbol <A,B,C> extends PullSymbol {
  public addSpecialization<A,B,C>(signature: PullSignatureSymbol<A,B,C>, typeArguments: PullTypeSymbol<any,any,any>[]): void;
  }
  class PullTypeSymbol <A,B,C> extends PullSymbol {
    public findTypeParameter<A,B,C>(name: string): PullTypeParameterSymbol<A,B,C>;
  }
  class PullTypeParameterSymbol <A,B,C> extends PullTypeSymbol {
    //~^ ERROR: Generic type 'TypeScript.PullTypeSymbol<A, B, C>' requires 3 type arguments.
  }
}
 
