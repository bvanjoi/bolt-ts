// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/errorOnEnumReferenceInCondition.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

enum Nums {
    Zero = 0,
    One = 1,
}

const a = Nums.Zero ? "a" : "b";    //~ERROR: This condition will always return 'false'.
const b = Nums.One ? "a" : "b";     //~ERROR: This condition will always return 'true'.

if (Nums.Zero) {    //~ERROR: This condition will always return 'false'.
    Nums;
}
else {
    Nums;
}


if (Nums.One) {     //~ERROR: This condition will always return 'true'.
    Nums;
}
else {
    Nums;
}


enum Strs {
    Empty = "",
    A = "A",
}

const c = Strs.Empty ? "a" : "b";   //~ERROR: This condition will always return 'false'.
const d = Strs.A ? "a" : "b";       //~ERROR: This condition will always return 'true'.

if (Strs.Empty) {                   //~ERROR: This condition will always return 'false'.
    Strs;
}
else {
    Strs;
}


if (Strs.A) {                       //~ERROR: This condition will always return 'true'.
    Strs;
}
else {
    Strs;
}