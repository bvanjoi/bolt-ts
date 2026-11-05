// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/doYouNeedToChangeYourTargetLibraryES2023.ts`, Apache-2.0 License

//@compiler-options: noEmit
//@[target=ES5]     compiler-options: target=es5
//@[target=ESNext]  compiler-options: target=esnext
//@compiler-options: strict

export let array = [0, 2, 3, 4];

function somePredicate(x: any): boolean {
    return x === 4;
}

export let lastIndex1 = array.findLastIndex(v => v === 4);
//~[target=ES5]^ ERROR: Property 'findLastIndex' does not exist on type 'number[]'.
//~[target=ES5]| ERROR: Parameter 'v' implicitly has an 'any' type.
export let lastIndex2 = array.findLastIndex(somePredicate);
//~[target=ES5]^ ERROR: Property 'findLastIndex' does not exist on type 'number[]'.

export let last1 = array.findLast(somePredicate);
//~[target=ES5]^ ERROR: Property 'findLast' does not exist on type 'number[]'.
export let last2 = array.findLast(somePredicate);
//~[target=ES5]^ ERROR: Property 'findLast' does not exist on type 'number[]'.

export let sorted = array.toSorted();
//~[target=ES5]^ ERROR: Property 'toSorted' does not exist on type 'number[]'.

export let spliced1 = array.toSpliced(2);
//~[target=ES5]^ ERROR: Property 'toSpliced' does not exist on type 'number[]'.
export let spliced2 = array.toSpliced(2, 1);
//~[target=ES5]^ ERROR: Property 'toSpliced' does not exist on type 'number[]'.
export let spliced3 = array.toSpliced(2, 1, 4, 5, 6, 7);
//~[target=ES5]^ ERROR: Property 'toSpliced' does not exist on type 'number[]'.
export let spliced4 = array.toSpliced(2, undefined);
//~[target=ES5]^ ERROR: Property 'toSpliced' does not exist on type 'number[]'.
export let spliced5 = array.toSpliced(2, undefined, 4, 5, 6, 7);
//~[target=ES5]^ ERROR: Property 'toSpliced' does not exist on type 'number[]'.
//~[target=ESNext]^^ ERROR: Argument of type 'undefined' is not assignable to parameter of type 'number'.

export let startsWithOne = array.with(0, 1);
//~[target=ES5]^ ERROR: Property 'with' does not exist on type 'number[]'.
export let inTheEnd = array.with(-1, 1);
//~[target=ES5]^ ERROR: Property 'with' does not exist on type 'number[]'.