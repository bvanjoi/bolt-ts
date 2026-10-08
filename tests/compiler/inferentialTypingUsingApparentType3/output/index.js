// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/inferentialTypingUsingApparentType3.ts`, Apache-2.0 License
class CharField {
  clean(input) {
    return 'Yup';
  }
}
class NumberField {
  clean(input) {
    return 123;
  }
}
class ObjectField {
  constructor(fields) {
    this.fields = fields}
}
var person = new ObjectField({
  id: new NumberField(),
  name: new CharField()  
});
person.fields.id;