// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/extendedUnicodePlaneIdentifiers.ts`, Apache-2.0 License
//@compiler-options: target=es2018
var 𝑚 = 4;
var 𝑀 = 5;
console.log(𝑀 + 𝑚);
// 9
class K {
  #𝑚 = 4;
  #𝑀 = 5;
}
// lower 8 bits look like 'a'
var ၡ = 6;
console.log(ၡ ** ၡ);
// lower 8 bits aren't a valid unicode character
var ဒ = 7;
console.log(ဒ ** ဒ);
// a mix, for good measure
var ဒၡ𝑀 = 7;
console.log(ဒၡ𝑀 ** ဒၡ𝑀);
var ၡ𝑀ဒ = 7;
console.log(ၡ𝑀ဒ ** ၡ𝑀ဒ);
var 𝑀ဒၡ = 7;
console.log(𝑀ဒၡ ** 𝑀ဒၡ);
var 𝓱𝓮𝓵𝓵𝓸 = '𝔀𝓸𝓻𝓵𝓭';
var Ɐⱱ = 'ok';
// BMP
var 𓀸𓀹𓀺 = 'ok';
// SMP
var 𡚭𡚮𡚯 = 'ok';
// SIP
var 𡚭𓀺ⱱ𝓮 = 'ok';
var 𓀺ⱱ𝓮𡚭 = 'ok';
var ⱱ𝓮𡚭𓀺 = 'ok';
var 𝓮𡚭𓀺ⱱ = 'ok';