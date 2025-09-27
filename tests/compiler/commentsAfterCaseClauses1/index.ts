// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/commentsAfterCaseClauses1.ts`, Apache-2.0 License

function getSecurity(level) {
    switch(level){
        case 0: // Zero
        case 1: // one
        case 2: // two
            return "Hi";
        case 3: // three
        case 4   : // four
            return "hello";
        case 5: // five
        default:  // default
            return "world";
    }
}