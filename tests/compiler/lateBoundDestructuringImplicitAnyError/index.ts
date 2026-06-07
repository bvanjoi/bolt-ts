// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/lateBoundDestructuringImplicitAnyError.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: lib=[es6]
//@compiler-options: noImplicitAny

let named = "foo";
let {[named]: prop} = {prop: "foo"};
//~^ ERROR: Type '{ prop: string; }' has no matching index signature for type 'string'.
void prop;

const numIndexed: {[idx: number]: string} = null as any;
const strIndexed: {[idx: string]: string} = null as any;

let numed = 6;

const symed = Symbol();
let symed2 = Symbol();

let {[named]: prop2} = numIndexed;
//~^ ERROR: Type '{ [idx: number]: string }' has no matching index signature for type 'string'.
void prop2;
let {[numed]: prop3} = numIndexed;
void prop3;
let {[named]: prop4} = strIndexed;
void prop4;
let {[numed]: prop5} = strIndexed;
void prop5;
let {[symed]: prop6} = numIndexed;
//~^ ERROR: Type 'unique symbol' cannot be used as an index type.
void prop6;
let {[symed]: prop7} = strIndexed;
//~^ ERROR: Type 'unique symbol' cannot be used as an index type.
void prop7;
let {[symed2]: prop8} = numIndexed;
//~^ ERROR: Type 'symbol' cannot be used as an index type.
void prop8;
let {[symed2]: prop9} = strIndexed;
//~^ ERROR: Type 'symbol' cannot be used as an index type.
void prop9;