// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/indexAt.ts`, Apache-2.0 License

//@[target=ES2021]  compiler-options: target=es2021
//@[target=ES2022]  compiler-options: target=es2022
//@[target=ESNext]  compiler-options: target=esnext

[0].at(0);
//~[target=ES2021]^ ERROR: Property 'at' does not exist on type 'number[]'.
"foo".at(0);
//~[target=ES2021]^ ERROR: Property 'at' does not exist on type '"foo"'.
new Int8Array().at(0);
//~[target=ES2021]^ ERROR: Property 'at' does not exist on type 'Int8Array<ArrayBuffer>'.
new Uint8Array().at(0);
//~[target=ES2021]^ ERROR: Property 'at' does not exist on type 'Uint8Array<ArrayBuffer>'.
new Uint8ClampedArray().at(0);
//~[target=ES2021]^ ERROR: Property 'at' does not exist on type 'Uint8ClampedArray<ArrayBuffer>'.
new Int16Array().at(0);
//~[target=ES2021]^ ERROR: Property 'at' does not exist on type 'Int16Array<ArrayBuffer>'.
new Uint16Array().at(0);
//~[target=ES2021]^ ERROR: Property 'at' does not exist on type 'Uint16Array<ArrayBuffer>'.
new Int32Array().at(0);
//~[target=ES2021]^ ERROR: Property 'at' does not exist on type 'Int32Array<ArrayBuffer>'.
new Uint32Array().at(0);
//~[target=ES2021]^ ERROR: Property 'at' does not exist on type 'Uint32Array<ArrayBuffer>'.
new Float32Array().at(0);
//~[target=ES2021]^ ERROR: Property 'at' does not exist on type 'Float32Array<ArrayBuffer>'.
new Float64Array().at(0);
//~[target=ES2021]^ ERROR: Property 'at' does not exist on type 'Float64Array<ArrayBuffer>'.
new BigInt64Array().at(0);
//~[target=ES2021]^ ERROR: Property 'at' does not exist on type 'BigInt64Array<ArrayBuffer>'.
new BigUint64Array().at(0);
//~[target=ES2021]^ ERROR: Property 'at' does not exist on type 'BigUint64Array<ArrayBuffer>'.
