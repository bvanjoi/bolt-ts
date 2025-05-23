// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/typeParameterOrderReversal.ts`, Apache-2.0 License

interface X<T> {
  n: T;
}

// Only difference here is order of type parameters
function uFirst<U extends X<T>, T>(x: U) { }
function tFirst<T, U extends X<T>>(x: U) { }
var z: X<number> = null;

// Both of these should be allowed
uFirst(z);
tFirst(z);
