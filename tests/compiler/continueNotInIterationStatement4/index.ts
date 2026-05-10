// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/continueNotInIterationStatement4.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: allowUnusedLabels

TWO:
while (true){
  var x = () => {
    continue TWO;
    //~^ ERROR: Jump target cannot cross function boundary.
    //~| ERROR: 'continue' statement can only be used within an enclosing iteration statement.
  }
}
