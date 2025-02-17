// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/protoAssignment.ts`, Apache-2.0 License

interface Number extends Comparable<number> {
  //~^ ERROR: Cannot find name 'Comparable'.

  compareTo(other: number);

}

Number.prototype.compareTo = function (other: number) {

 return this.valueOf() == other;

}


