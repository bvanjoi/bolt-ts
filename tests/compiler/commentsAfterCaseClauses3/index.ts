// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/commentsAfterCaseClauses3.ts`, Apache-2.0 License

//@compiler-options: strict=false
//@compiler-options: target=es2015

function getSecurity(level) {
    switch(level){
        case 0: /*Zero*/
        case 1: /*One*/ 
        case 2: /*two*/
            // Leading comments
            return "Hi";
        case 3: /*three*/
        case 4: /*four*/
            return "hello";
        case 5: /*five*/
        default:  /*six*/
            return "world";
    }
    
}