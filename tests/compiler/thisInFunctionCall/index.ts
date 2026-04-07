// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/thisInFunctionCall.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitThis

class Test {
  data: number[]

  constructor() {
    this.data = [1, 2, 3]
  }

  finderRaw() {
    this.data.find(function (d) {
      return d === this.data.length
      //~^ ERROR: 'this' implicitly has type 'any' because it does not have a type annotation.
      //~| ERROR: 'this' implicitly has type 'any' because it does not have a type annotation.
    })
  }

  forEacherRaw() {
    this.data.forEach(function (d) {
      console.log(d === this.data.length)
      //~^ ERROR: 'this' implicitly has type 'any' because it does not have a type annotation.
    })
  }

  forEacher() {
    this.data.forEach(
    /** @this {Test} */
    function (d) {
      console.log(d === this.data.length)
      //~^ ERROR: 'this' implicitly has type 'any' because it does not have a type annotation.
    }, this)
  }

  finder() {
    this.data.find(
    /** @this {Test} */
    function (d) {
      return d === this.data.length
      //~^ ERROR: 'this' implicitly has type 'any' because it does not have a type annotation.
      //~| ERROR: 'this' implicitly has type 'any' because it does not have a type annotation.
    }, this)
  }
}