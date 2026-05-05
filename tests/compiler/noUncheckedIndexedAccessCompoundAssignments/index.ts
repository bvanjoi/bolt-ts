// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noUncheckedIndexedAccessCompoundAssignments.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noUncheckedIndexedAccess

stringMap.foo++;
//~^ ERROR: 'stringMap.foo' is possibly 'undefined'.
--stringMap.foo;
//~^ ERROR: 'stringMap.foo' is possibly 'undefined'.
stringMap.foo += 1;
//~^ ERROR: Operator '+=' cannot be applied to types 'undefined | number' and '1'.
stringMap.foo *= 1;
//~^ ERROR: 'stringMap.foo' is possibly 'undefined'.
++stringMap['foo'];
//~^ ERROR: 'stringMap[foo]' is possibly 'undefined'.
stringMap['foo']--;
//~^ ERROR: 'stringMap[foo]' is possibly 'undefined'.
++stringMap[s];
//~^ ERROR: 'stringMap[s]' is possibly 'undefined'.
stringMap[s]--;
//~^ ERROR: 'stringMap[s]' is possibly 'undefined'.
numberMap[32]++;
//~^ ERROR: 'numberMap[32]' is possibly 'undefined'.
numberMap[32] += 1;
//~^ ERROR: Operator '+=' cannot be applied to types 'undefined | number' and '1'.
numberMap[n]++;
//~^ ERROR: 'numberMap[n]' is possibly 'undefined'.
numberMap[n] += 1;
//~^ ERROR: Operator '+=' cannot be applied to types 'undefined | number' and '1'.

declare const stringMap: { [s: string]: number };
declare const s: string;
declare const numberMap: { [n: number]: number };
declare const n: number;

const a: number = stringMap.foo;
//~^ ERROR: Type 'undefined | number' is not assignable to type 'number'.
