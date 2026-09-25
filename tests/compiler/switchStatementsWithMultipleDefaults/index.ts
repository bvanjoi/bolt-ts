// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/switchStatementsWithMultipleDefaults.ts`, Apache-2.0 License

//@compiler-options: target=es2015

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

switch (x) {
    default:    // No issues.
        break;
    case 100:
        switch (x * x) {
            default:    // No issues.
            default:    // Error; second 'default' clause.
    //~^ ERROR: A 'default' clause cannot appear more than once in a 'switch' statement.
                break;
            case 10000:
                x /= x;
            default:    // Error, third 'default' clause
    //~^ ERROR: A 'default' clause cannot appear more than once in a 'switch' statement.
            def\u0061ult: // Error, fourth 'default' clause.
    //~^ ERROR: A 'default' clause cannot appear more than once in a 'switch' statement.
    //~| ERROR: Keywords cannot contain escape characters.
            // Errors on fifth-seventh
            default: return;
    //~^ ERROR: A 'default' clause cannot appear more than once in a 'switch' statement.
    //~| ERROR: A 'return' statement can only be used within a function body.
            default: default:
    //~^ ERROR: A 'default' clause cannot appear more than once in a 'switch' statement.
    //~| ERROR: A 'default' clause cannot appear more than once in a 'switch' statement.
        }
}