// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/memberOverride.ts`, Apache-2.0 License

var x = {
  a: "", 
  a: 5
  //~^ ERROR: Duplicate identifier 'a'.
}

var n: number = x.a;