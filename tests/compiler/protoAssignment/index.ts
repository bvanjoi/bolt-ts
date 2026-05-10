// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/protoAssignment.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

interface Number extends Comparable<number> {
  //~^ ERROR: Cannot find name 'Comparable'.

    compareTo(other: number);

}

Number.prototype.compareTo = function (other: number) {

   return this.valueOf() == other;

}

 
