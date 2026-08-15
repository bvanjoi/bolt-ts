// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeGuardConstructorClassAndNumber.ts`, Apache-2.0 License

//@compiler-options: target=es2015

// Typical case
class C1 {
    property1!: string;
}

declare let var1: C1 | number;
if (var1.constructor == C1) {
    var1; // C1
    var1.property1; // string
    const a: 42 = var1.property1;
    //~^ ERROR: Type 'string' is not assignable to type '42'.
}
else {
    var1; // number | C1
}
if (var1["constructor"] == C1) {
    var1; // C1
    var1.property1; // string
    const a: 42 = var1.property1;
    //~^ ERROR: Type 'string' is not assignable to type '42'.
}
else {
    var1; // number | C1
}
if (var1.constructor === C1) {
    var1; // C1
    var1.property1; // string
    const a: 42 = var1.property1;
    //~^ ERROR: Type 'string' is not assignable to type '42'.
}
else {
    var1; // number | C1
}
if (var1["constructor"] === C1) {
    var1; // C1
    var1.property1; // string
    const a: 42 = var1.property1;
    //~^ ERROR: Type 'string' is not assignable to type '42'.
}
else {
    var1; // number | C1
}
if (C1 == var1.constructor) {
    var1; // C1
    var1.property1; // string
    const a: 42 = var1.property1;
    //~^ ERROR: Type 'string' is not assignable to type '42'.
}
else {
    var1; // number | C1
}
if (C1 == var1["constructor"]) {
    var1; // C1
    var1.property1; // string
    const a: 42 = var1.property1;
    //~^ ERROR: Type 'string' is not assignable to type '42'.
}
else {
    var1; // number | C1
}
if (C1 === var1.constructor) {
    var1; // C1
    var1.property1; // string
    const a: 42 = var1.property1;
    //~^ ERROR: Type 'string' is not assignable to type '42'.
}
else {
    var1; // number | C1
}
if (C1 === var1["constructor"]) {
    var1; // C1
    var1.property1; // string
    const a: 42 = var1.property1;
    //~^ ERROR: Type 'string' is not assignable to type '42'.
}
else {
    var1; // number | C1
}

if (var1.constructor != C1) {
    var1; // C1 | number
    var1.property1; // error
    //~^ ERROR: Property 'property1' does not exist on type 'number | C1'.
}
else {
    var1; // C1
}
if (var1["constructor"] != C1) {
    var1; // C1 | number
    var1.property1; // error
    //~^ ERROR: Property 'property1' does not exist on type 'number | C1'.
}
else {
    var1; // C1
}
if (var1.constructor !== C1) {
    var1; // C1 | number
    var1.property1; // error
    //~^ ERROR: Property 'property1' does not exist on type 'number | C1'.
}
else {
    var1; // C1
}
if (var1["constructor"] !== C1) {
    var1; // C1 | number
    var1.property1; // error
    //~^ ERROR: Property 'property1' does not exist on type 'number | C1'.
}
else {
    var1; // C1
}
if (C1 != var1.constructor) {
    var1; // C1 | number
    var1.property1; // error
    //~^ ERROR: Property 'property1' does not exist on type 'number | C1'.
}
else {
    var1; // C1
}
if (C1 != var1["constructor"]) {
    var1; // C1 | number
    var1.property1; // error
    //~^ ERROR: Property 'property1' does not exist on type 'number | C1'.
}
else {
    var1; // C1
}
if (C1 !== var1.constructor) {
    var1; // C1 | number
    var1.property1; // error
    //~^ ERROR: Property 'property1' does not exist on type 'number | C1'.
}
else {
    var1; // C1
}
if (C1 !== var1["constructor"]) {
    var1; // C1 | number
    var1.property1; // error
    //~^ ERROR: Property 'property1' does not exist on type 'number | C1'.
}
else {
    var1; // C1
}

// Repro from #37660

function foo(instance: Function | object) {
    if (typeof instance === 'function') {
        if (instance.prototype == null || instance.prototype.constructor == null) {
            return instance.length;
        }
    }
}
