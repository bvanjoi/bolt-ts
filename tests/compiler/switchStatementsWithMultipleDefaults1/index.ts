// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/switchStatementsWithMultipleDefaults1.ts`, Apache-2.0 License

//@compiler-options: target=esnext


    var x = 10;
    
    switch (x) {
        case 1:
        case 2:
        default:    // No issues.
            break;
        default:    // Error; second 'default' clause.
        //~^ ERROR: A 'default' clause cannot appear more than once in a 'switch' statement.
        default:    // Error; third 'default' clause.
        //~^ ERROR: A 'default' clause cannot appear more than once in a 'switch' statement.
        case 3:
            x *= x;
    }

