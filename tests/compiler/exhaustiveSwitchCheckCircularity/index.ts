// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/exhaustiveSwitchCheckCircularity.ts`, Apache-2.0 License

//@compiler-options: strict

// Repro from #47539

declare function isNever(n: never): boolean;

function f() {
    let foo: "aaa" | "bbb" = "aaa";
    while (true) {
        switch (foo) {
            case "aaa":
        }
        if (foo === "aaa") {
            foo = "bbb";
        }
        else if (isNever(foo)) {  // Error expected
          //~^ ERROR: Argument of type 'string' is not assignable to parameter of type 'never'.
            break;
        }
    }
}

// Repro from #51688

declare function functionB(key: string): string;

function functionC(): void {
    let unionVal: "A" | "B" = "A";
    while (true) {
        let key: string;
        switch (unionVal) {
            case "A": {
                key = "AA";
                break;
            }
        }
        functionB(key);
    }
}
