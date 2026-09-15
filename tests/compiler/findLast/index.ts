// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/findLast.ts`, Apache-2.0 License

//@compiler-options: strict=false
//@[target=ES2022]  compiler-options: target=es2022
//@[target=ESNext]  compiler-options: target=esnext

const itemNumber: number | undefined = [0].findLast((item) => item === 0);
//~[target=ES2022]^ ERROR: Property 'findLast' does not exist on type 'number[]'.
const itemString: string | undefined = ["string"].findLast((item) => item === "string");
//~[target=ES2022]^ ERROR: Property 'findLast' does not exist on type 'string[]'.
new Int8Array().findLast((item) => item === 0);
//~[target=ES2022]^ ERROR: Property 'findLast' does not exist on type 'Int8Array<ArrayBuffer>'.
new Uint8Array().findLast((item) => item === 0);
//~[target=ES2022]^ ERROR: Property 'findLast' does not exist on type 'Uint8Array<ArrayBuffer>'.
new Uint8ClampedArray().findLast((item) => item === 0);
//~[target=ES2022]^ ERROR: Property 'findLast' does not exist on type 'Uint8ClampedArray<ArrayBuffer>'.
new Int16Array().findLast((item) => item === 0);
//~[target=ES2022]^ ERROR: Property 'findLast' does not exist on type 'Int16Array<ArrayBuffer>'.
new Uint16Array().findLast((item) => item === 0);
//~[target=ES2022]^ ERROR: Property 'findLast' does not exist on type 'Uint16Array<ArrayBuffer>'.
new Int32Array().findLast((item) => item === 0);
//~[target=ES2022]^ ERROR: Property 'findLast' does not exist on type 'Int32Array<ArrayBuffer>'.
new Uint32Array().findLast((item) => item === 0);
//~[target=ES2022]^ ERROR: Property 'findLast' does not exist on type 'Uint32Array<ArrayBuffer>'.
new Float32Array().findLast((item) => item === 0);
//~[target=ES2022]^ ERROR: Property 'findLast' does not exist on type 'Float32Array<ArrayBuffer>'.
new Float64Array().findLast((item) => item === 0);
//~[target=ES2022]^ ERROR: Property 'findLast' does not exist on type 'Float64Array<ArrayBuffer>'.
new BigInt64Array().findLast((item) => item === BigInt(0));
//~[target=ES2022]^ ERROR: Property 'findLast' does not exist on type 'BigInt64Array<ArrayBuffer>'.
new BigUint64Array().findLast((item) => item === BigInt(0));
//~[target=ES2022]^ ERROR: Property 'findLast' does not exist on type 'BigUint64Array<ArrayBuffer>'.

const indexNumber: number = [0].findLastIndex((item) => item === 0);
//~[target=ES2022]^ ERROR: Property 'findLastIndex' does not exist on type 'number[]'.
const indexString: number = ["string"].findLastIndex((item) => item === "string");
//~[target=ES2022]^ ERROR: Property 'findLastIndex' does not exist on type 'string[]'.
new Int8Array().findLastIndex((item) => item === 0);
//~[target=ES2022]^ ERROR: Property 'findLastIndex' does not exist on type 'Int8Array<ArrayBuffer>'.
new Uint8Array().findLastIndex((item) => item === 0);
//~[target=ES2022]^ ERROR: Property 'findLastIndex' does not exist on type 'Uint8Array<ArrayBuffer>'.
new Uint8ClampedArray().findLastIndex((item) => item === 0);
//~[target=ES2022]^ ERROR: Property 'findLastIndex' does not exist on type 'Uint8ClampedArray<ArrayBuffer>'.
new Int16Array().findLastIndex((item) => item === 0);
//~[target=ES2022]^ ERROR: Property 'findLastIndex' does not exist on type 'Int16Array<ArrayBuffer>'.
new Uint16Array().findLastIndex((item) => item === 0);
//~[target=ES2022]^ ERROR: Property 'findLastIndex' does not exist on type 'Uint16Array<ArrayBuffer>'.
new Int32Array().findLastIndex((item) => item === 0);
//~[target=ES2022]^ ERROR: Property 'findLastIndex' does not exist on type 'Int32Array<ArrayBuffer>'.
new Uint32Array().findLastIndex((item) => item === 0);
//~[target=ES2022]^ ERROR: Property 'findLastIndex' does not exist on type 'Uint32Array<ArrayBuffer>'.
new Float32Array().findLastIndex((item) => item === 0);
//~[target=ES2022]^ ERROR: Property 'findLastIndex' does not exist on type 'Float32Array<ArrayBuffer>'.
new Float64Array().findLastIndex((item) => item === 0);
//~[target=ES2022]^ ERROR: Property 'findLastIndex' does not exist on type 'Float64Array<ArrayBuffer>'.
new BigInt64Array().findLastIndex((item) => item === BigInt(0));
//~[target=ES2022]^ ERROR: Property 'findLastIndex' does not exist on type 'BigInt64Array<ArrayBuffer>'.
new BigUint64Array().findLastIndex((item) => item === BigInt(0));
//~[target=ES2022]^ ERROR: Property 'findLastIndex' does not exist on type 'BigUint64Array<ArrayBuffer>'.
