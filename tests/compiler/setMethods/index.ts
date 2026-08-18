// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/setMethods.ts`, Apache-2.0 License

//@compiler-options: strict=false
//@compiler-options: target=esnext

let numberSet = new Set([0, 1, 2]);

let stringSet = new Set(["a", "b"]);

let numberMap = new Map([[4, {}], [5, {}]]);

let numberSetLike = {
  size: 1,
  *keys() { yield 3 },
  has(x) { return x === 3 },
};

numberSet.union([]);
//~^ ERROR: Property 'has' is missing.
//~| ERROR: Property 'size' is missing.
numberSet.union(new Set);
numberSet.union(stringSet);
numberSet.union(numberMap);
numberSet.union(numberSetLike);

numberSet.intersection([]);
//~^ ERROR: Property 'has' is missing.
//~| ERROR: Property 'size' is missing.
numberSet.intersection(new Set);
numberSet.intersection(stringSet);
numberSet.intersection(numberMap);
numberSet.intersection(numberSetLike);

numberSet.difference([]);
//~^ ERROR: Property 'has' is missing.
//~| ERROR: Property 'size' is missing.
numberSet.difference(new Set);
numberSet.difference(stringSet);
numberSet.difference(numberMap);
numberSet.difference(numberSetLike);

numberSet.symmetricDifference([]);
//~^ ERROR: Property 'has' is missing.
//~| ERROR: Property 'size' is missing.
numberSet.symmetricDifference(new Set);
numberSet.symmetricDifference(stringSet);
numberSet.symmetricDifference(numberMap);
numberSet.symmetricDifference(numberSetLike);

numberSet.isSubsetOf([]);
//~^ ERROR: Property 'has' is missing.
//~| ERROR: Property 'size' is missing.
numberSet.isSubsetOf(new Set);
numberSet.isSubsetOf(stringSet);
numberSet.isSubsetOf(numberMap);
numberSet.isSubsetOf(numberSetLike);

numberSet.isSupersetOf([]);
//~^ ERROR: Property 'has' is missing.
//~| ERROR: Property 'size' is missing.
numberSet.isSupersetOf(new Set);
numberSet.isSupersetOf(stringSet);
numberSet.isSupersetOf(numberMap);
numberSet.isSupersetOf(numberSetLike);

numberSet.isDisjointFrom([]);
//~^ ERROR: Property 'has' is missing.
//~| ERROR: Property 'size' is missing.
numberSet.isDisjointFrom(new Set);
numberSet.isDisjointFrom(stringSet);
numberSet.isDisjointFrom(numberMap);
numberSet.isDisjointFrom(numberSetLike);
