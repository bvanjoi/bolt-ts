// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/conditionalDoesntLeakUninstantiatedTypeParameter.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface Synthetic<A, B extends A> {}
type SyntheticDestination<T, U> = U extends Synthetic<T, infer V> ? V : never;
type TestSynthetic = // Resolved to T, should be `number` or an inference failure (`unknown`)
    SyntheticDestination<number, Synthetic<number, number>>;

const y: TestSynthetic = 3; // Type '3' is not assignable to type 'T'. (shouldn't error)
const z: TestSynthetic = '3'; // Type '"3""' is not assignable to type 'T'. (should not mention T)
//~^ ERROR: Type 'string' is not assignable to type 'number'.
