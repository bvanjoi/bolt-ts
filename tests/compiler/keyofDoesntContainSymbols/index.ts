// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/keyofDoesntContainSymbols.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: lib=[es6]
//@compiler-options: keyofStringsOnly

const sym = Symbol();
const num = 0;
const obj = { num: 0, str: 's', [num]: num as 0, [sym]: sym };

function set <T extends object, K extends keyof T> (obj: T, key: K, value: T[K]): T[K] {
  return obj[key] = value;
}

const val = set(obj, 'str', '');
// string
const valB = set(obj, 'num', '');
//~^ ERROR: Argument of type 'string' is not assignable to parameter of type 'number'.
// Expect type error
// Argument of type '""' is not assignable to parameter of type 'number'.
const valC = set(obj, sym, sym);
// Expect type error
// Argument of type 'unique symbol' is not assignable to parameter of type "str" | "num"
const valD = set(obj, num, num);
// Expect type error
// Argument of type '0' is not assignable to parameter of type "str" | "num"
type KeyofObj = keyof typeof obj;
// "str" | "num"
type Values<T> = T[keyof T];

type ValuesOfObj = Values<typeof obj>;