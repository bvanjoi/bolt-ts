// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/typeParametersAndParametersInComputedNames.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function foo<T>(a: T) : string {
    return "";
}

class A {
    [foo<T>(a)]<T>(a: T) {  
        //~^ ERROR: Cannot find name 'T'.
        //~| ERROR: Cannot find name 'a'.
    }
}