// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/memberOverride.ts`, Apache-2.0 License

var x = {
  a: "", 
  a: 5
  //~^ ERROR: An object literal cannot have multiple properties with the same name.
}

var n: number = x.a;