// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/genericTypeWithCallableMembers.ts`, Apache-2.0 License
class C {
  constructor(data, data2) {
    this.data = data
    
    this.data2 = data2}
  create() {
    // no error
    var x = new this.data();
    // was error, shouldn't be
    var x2 = new this.data2();
  }
}