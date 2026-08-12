// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/indexedAccessWithFreshObjectLiteral.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

function foo (id: string) {
  return {
      a: 1,
      b: "",
      c: true
  }[id]
}

function bar (id: 'a' | 'b') {
  return {
      a: 1,
      b: "",
      c: false
  }[id]
}

function baz (id: '1' | '2') {
  return {
      1: 1,
      2: "",
      3: false
  }[id]
}

function qux (id: 1 | 2) {
  return {
      1: 1,
      2: "",
      3: false
  }[id]
}

function quux (id: 'a' | 'b' | 'z') {
  return {
    //~^ ERROR: Property '"z"' does not exist on type '{ a: number; b: string; c: boolean; }'.
      a: 1,
      b: "",
      c: false
  }[id]
}

function corge(id: string) {
  return ({
      a: 123,
      b: ""
  } as Record<string, number | string>)[id]
}

function grault(id: string) {
  return ({
      a: 123,
      b: ""
  } as { [k: string]: string | number})[id]
}
