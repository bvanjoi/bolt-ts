// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedSwitchStatement.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

switch (1) {
    case 0:
      //~^ ERROR: Type '0' is not comparable to type '1'.
        let x;
        //~^ ERROR: 'x' is declared but its value is never read.
        break;
    case 1:
        const c = 1;
        //~^ ERROR: 'c' is declared but its value is never read.
        break;
    default:
        let z = 2;
        //~^ ERROR: 'z' is declared but its value is never read.
}


switch (2) {
    case 0:
      //~^ ERROR: Type '0' is not comparable to type '2'.
        let x;
        //~^ ERROR: 'x' is declared but its value is never read.
    case 1:
      //~^ ERROR: Type '1' is not comparable to type '2'.
        x=1;
}