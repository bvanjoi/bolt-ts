// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/specialIntersectionsInMappedTypes.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noUncheckedIndexedAccess

// Repro from #50683

type Alignment = (string & {}) | "left" | "center" | "right";
type Alignments = Record<Alignment, string>;

const a: Alignments = {
    left: "align-left",
    center: "align-center",
    right: "align-right",
    other: "align-other",
};

a.other;
a.left.length;
a.other.length;  // Error expected here
//~^ ERROR: 'a.other' is possibly undefined.

const e: number[][][] = [];
e[0][0][0];
//~^ ERROR: 'e[0]' is possibly undefined.
//~| ERROR: 'e[0][0]' is possibly undefined.

e?.[0]?.[0]?.[0];