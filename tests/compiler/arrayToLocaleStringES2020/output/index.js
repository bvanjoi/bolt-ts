var str;
var arr = [1, 2, 3];
str = arr.toLocaleString();
str = arr.toLocaleString('en-US');
str = arr.toLocaleString('en-US', {
  style: 'currency',
  currency: 'EUR'  
});
var dates = [new Date(), new Date()];
str = dates.toLocaleString();
str = dates.toLocaleString('fr');
str = dates.toLocaleString('fr', {
  timeZone: 'UTC'  
});
var mixed = [1, new Date(), 59782, new Date()];
str = mixed.toLocaleString();
str = mixed.toLocaleString('fr');
str = mixed.toLocaleString('de', {
  style: 'currency',
  currency: 'EUR'  
});
str = (mixed).toLocaleString('de', {
  currency: 'EUR',
  style: 'currency',
  timeZone: 'UTC'  
});
var int8Array = new Int8Array(3);
str = int8Array.toLocaleString();
str = int8Array.toLocaleString('en-US');
str = int8Array.toLocaleString('en-US', {
  style: 'currency',
  currency: 'EUR'  
});
var uint8Array = new Uint8Array(3);
str = uint8Array.toLocaleString();
str = uint8Array.toLocaleString('en-US');
str = uint8Array.toLocaleString('en-US', {
  style: 'currency',
  currency: 'EUR'  
});
var uint8ClampedArray = new Uint8ClampedArray(3);
str = uint8ClampedArray.toLocaleString();
str = uint8ClampedArray.toLocaleString('en-US');
str = uint8ClampedArray.toLocaleString('en-US', {
  style: 'currency',
  currency: 'EUR'  
});
var int16Array = new Int16Array(3);
str = int16Array.toLocaleString();
str = int16Array.toLocaleString('en-US');
str = int16Array.toLocaleString('en-US', {
  style: 'currency',
  currency: 'EUR'  
});
var uint16Array = new Uint16Array(3);
str = uint16Array.toLocaleString();
str = uint16Array.toLocaleString('en-US');
str = uint16Array.toLocaleString('en-US', {
  style: 'currency',
  currency: 'EUR'  
});
var int32Array = new Int32Array(3);
str = int32Array.toLocaleString();
str = int32Array.toLocaleString('en-US');
str = int32Array.toLocaleString('en-US', {
  style: 'currency',
  currency: 'EUR'  
});
var uint32Array = new Uint32Array(3);
str = uint32Array.toLocaleString();
str = uint32Array.toLocaleString('en-US');
str = uint32Array.toLocaleString('en-US', {
  style: 'currency',
  currency: 'EUR'  
});
var float32Array = new Float32Array(3);
str = float32Array.toLocaleString();
str = float32Array.toLocaleString('en-US');
str = float32Array.toLocaleString('en-US', {
  style: 'currency',
  currency: 'EUR'  
});
var float64Array = new Float64Array(3);
str = float64Array.toLocaleString();
str = float64Array.toLocaleString('en-US');
str = float64Array.toLocaleString('en-US', {
  style: 'currency',
  currency: 'EUR'  
});