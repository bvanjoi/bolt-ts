// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/parameterReferencesOtherParameter1.ts`, Apache-2.0 License

class Model {
  public name: string;
}

class UI {
  constructor(model: Model, foo:string = model.name)
  {
  }
}

class UI2 {
  constructor(model: Model, foo:number = model.name) //~ ERROR:  Type 'string' is not assignable to type 'number'.
  {
  }
}