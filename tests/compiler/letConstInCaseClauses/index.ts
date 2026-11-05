// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/letConstInCaseClauses.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

var x = 10;
var y = 20;
{
    let x = 1;
    let y = 2;
    console.log(x)
    switch (x) {
        case 10:
            let x = 20;
    }
    switch (y) {
        case 10:
            let y = 20;
    }
}

{
    const x = 1;
    const y = 2;
    console.log(x)
    switch (x) {
        case 10:
          //~^ ERROR: Type '10' is not comparable to type '1'.
            const x = 20;
    }
    switch (y) {
        case 10:
          //~^ ERROR: Type '10' is not comparable to type '2'.
            const y = 20;
    }
}
