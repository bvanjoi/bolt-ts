// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/checkingObjectWithThisInNamePositionNoCrash.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs
//@compiler-options: strict
//@compiler-options: declaration 

export const thing = {
    doit() {
    //~^ ERROR: 'doit' implicitly has return type 'any' because it does not have a return type annotation and is referenced directly or indirectly in one of its return expressions.
        return {
            [this.a]: "", // should refer to the outer object with the doit method, notably not present
            //~^ ERROR: Property 'a' does not exist on type '{ doit: () => error; }'.
        }
    }
}