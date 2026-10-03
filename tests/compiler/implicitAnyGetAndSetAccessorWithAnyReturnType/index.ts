// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/implicitAnyGetAndSetAccessorWithAnyReturnType.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitAny

class GetAndSet {
    getAndSet = null;             // error at "getAndSet"
    public get haveGetAndSet() {  // this should not be an error
        return this.getAndSet;
    }
    
    // this shouldn't be an error
    public set haveGetAndSet(value) {  // error at "value"
        this.getAndSet = value;
    }
}

class SetterOnly {
    public set haveOnlySet(newXValue) {  // error at "haveOnlySet, newXValue"
      //~^ ERROR: Property 'haveOnlySet' implicitly has type 'any', because its set accessor lacks a parameter type annotation.
      //~| ERROR: Parameter 'newXValue' implicitly has an 'any' type.
    }
}

class GetterOnly {
    public get haveOnlyGet() {  // error at "haveOnlyGet"
        return null;
    }
}