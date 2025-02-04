// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/arrayLiteralTypeInference.ts`, Apache-2.0 License

class Action {
  id: number;
}

class ActionA extends Action {
  value: string;
}

class ActionB extends Action {
  trueNess: boolean;
}

var x1: Action[] = [
  { id: 2, trueness: false },
  //~^ ERROR: Object literal may only specify known properties, and 'trueness' does not exist.
  { id: 3, name: "three" }
  //~^ ERROR: Object literal may only specify known properties, and 'name' does not exist.
]

var x2: Action[] = [
  new ActionA(),
  new ActionB()
]

var x3: Action[] = [
  new Action(),
  new ActionA(),
  new ActionB()
]

var z1: { id: number }[] =
  [
      { id: 2, trueness: false },
      //~^ ERROR: Object literal may only specify known properties, and 'trueness' does not exist.
      { id: 3, name: "three" }
      //~^ ERROR: Object literal may only specify known properties, and 'name' does not exist.
  ]

var z2: { id: number }[] =
  [
      new ActionA(),
      new ActionB()
  ]

var z3: { id: number }[] =
  [
      new Action(),
      new ActionA(),
      new ActionB()
  ]




