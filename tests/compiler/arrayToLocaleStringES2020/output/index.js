// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/accessorDeclarationEmitVisibilityErrors.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var str;
var arr = [1, 2, 3];
str = arr.toLocaleString();// OK

str = arr.toLocaleString('en-US');// OK

str = arr.toLocaleString('en-US', {
  style: 'currency',
  currency: 'EUR'  
});// OK

var dates = [new Date(), new Date()];
str = dates.toLocaleString();// OK

str = dates.toLocaleString('fr');// OK

str = dates.toLocaleString('fr', {
  timeZone: 'UTC'  
});// OK

var mixed = [1, new Date(), 59782, new Date()];
str = mixed.toLocaleString();// OK

str = mixed.toLocaleString('fr');// OK

str = mixed.toLocaleString('de', {
  style: 'currency',
  currency: 'EUR'  
});// OK

str = (mixed).toLocaleString('de', {
  currency: 'EUR',
  style: 'currency',
  timeZone: 'UTC'  
});// OK

var int8Array = new Int8Array(3);
str = int8Array.toLocaleString();// OK

str = int8Array.toLocaleString('en-US');// OK

str = int8Array.toLocaleString('en-US', {
  style: 'currency',
  currency: 'EUR'  
});// OK

var uint8Array = new Uint8Array(3);
str = uint8Array.toLocaleString();// OK

str = uint8Array.toLocaleString('en-US');// OK

str = uint8Array.toLocaleString('en-US', {
  style: 'currency',
  currency: 'EUR'  
});// OK

var uint8ClampedArray = new Uint8ClampedArray(3);
str = uint8ClampedArray.toLocaleString();// OK

str = uint8ClampedArray.toLocaleString('en-US');// OK

str = uint8ClampedArray.toLocaleString('en-US', {
  style: 'currency',
  currency: 'EUR'  
});// OK

var int16Array = new Int16Array(3);
str = int16Array.toLocaleString();// OK

str = int16Array.toLocaleString('en-US');// OK

str = int16Array.toLocaleString('en-US', {
  style: 'currency',
  currency: 'EUR'  
});// OK

var uint16Array = new Uint16Array(3);
str = uint16Array.toLocaleString();// OK

str = uint16Array.toLocaleString('en-US');// OK

str = uint16Array.toLocaleString('en-US', {
  style: 'currency',
  currency: 'EUR'  
});// OK

var int32Array = new Int32Array(3);
str = int32Array.toLocaleString();// OK

str = int32Array.toLocaleString('en-US');// OK

str = int32Array.toLocaleString('en-US', {
  style: 'currency',
  currency: 'EUR'  
});// OK

var uint32Array = new Uint32Array(3);
str = uint32Array.toLocaleString();// OK

str = uint32Array.toLocaleString('en-US');// OK

str = uint32Array.toLocaleString('en-US', {
  style: 'currency',
  currency: 'EUR'  
});// OK

var float32Array = new Float32Array(3);
str = float32Array.toLocaleString();// OK

str = float32Array.toLocaleString('en-US');// OK

str = float32Array.toLocaleString('en-US', {
  style: 'currency',
  currency: 'EUR'  
});// OK

var float64Array = new Float64Array(3);
str = float64Array.toLocaleString();// OK

str = float64Array.toLocaleString('en-US');// OK

str = float64Array.toLocaleString('en-US', {
  style: 'currency',
  currency: 'EUR'  
});