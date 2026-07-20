// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/errorsForCallAndAssignmentAreSimilar.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function minimalExample1() {
    type Disc =
        | { kind: "hddvd" }
        | { kind: "bluray" }

    function foo(x: Disc[]) {
    }

    foo([
        { kind: "bluray", },
        { kind: "hdpvd", }
        //~^ ERROR: Type '"hdpvd"' is not assignable to type '"hddvd" | "bluray"'.
    ]);

    const ds: Disc[] = [
        { kind: "bluray", },
        { kind: "hdpvd", }
        //~^ ERROR:  Type '"hdpvd"' is not assignable to type '"hddvd" | "bluray"'.
    ];
}