// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeArgumentInferenceWithConstraintAsCommonRoot.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

interface Animal { x }
interface Giraffe extends Animal { y }
interface Elephant extends Animal { z }
function f<T extends Animal>(x: T, y: T): T { return undefined; }
declare var g: Giraffe;
declare var e: Elephant;
f(g, e); // valid because both Giraffe and Elephant satisfy the constraint. T is Animal
//~^ ERROR: Property 'y' is missing.