// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/library_ObjectPrototypeProperties.ts`, Apache-2.0 License
//@compiler-options: target=es2015
// Properties of the Object Prototype Object as per ES5 spec
Object.prototype.constructor;
Object.prototype.toString();
Object.prototype.toLocaleString();
Object.prototype.valueOf();
Object.prototype.hasOwnProperty('string');
Object.prototype.isPrototypeOf(Object);
Object.prototype.propertyIsEnumerable('string');