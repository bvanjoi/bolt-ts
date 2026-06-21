// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contextualSignatureInstatiationCovariance.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

interface Animal { x }
interface TallThing { x2 }
interface Giraffe extends Animal, TallThing { y }

var f2: <T extends Giraffe>(x: T, y: T) => void;

var g2: (a: Animal, t: TallThing) => void;
g2 = f2; // While neither Animal nor TallThing satisfy the constraint, T is at worst a Giraffe and compatible with both via covariance.

var h2: (a1: Animal, a2: Animal) => void;
h2 = f2; // Animal does not satisfy the constraint, but T is at worst a Giraffe and compatible with Animal via covariance.