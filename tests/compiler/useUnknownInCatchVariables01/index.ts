// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/useUnknownInCatchVariables01.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: useUnknownInCatchVariables


try {
    // ...
}
catch (e) {
    // error!
    void e.toUpperCase();
    //~^ ERROR: Object is of type 'unknown'.
    void e++;
    //~^ ERROR: Object is of type 'unknown'.
    void e();
    //~^ ERROR: Object is of type 'unknown'.

    if (typeof e === "string") {
        // works!
        // We've narrowed 'e' down to the type 'string'.
        console.log(e.toUpperCase());
    }
    if (e instanceof Error) {
        e.stack?.toUpperCase();
    }
    if (typeof e === "number") {
        e.toExponential();
        e++;
    }
}


try {
    // ...
}
catch (e: any) {
    // All are allowed.
    void e.toUpperCase();
    void e.toExponential();
    void e();
}
