// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionsMissingReturnStatementsAndExpressions.ts`, Apache-2.0 License

//@compiler-options: allowUnreachableCode
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

function f1(): string {   //~ ERROR: A function whose declared type is neither 'undefined', 'void', nor 'any' must return a value.
    // errors because there are no return statements
}

function f2(): string {
    // Permissible; returns undefined.
    return;   //~ ERROR: Type 'undefined' is not assignable to type 'string'.
}

function f3(): string {
    return "Okay, because this is a return expression.";
}

function f4(): void {
    // Fine since we are typed void.
}

function f5(): void {
    // Fine since we are typed void.
    return;
}

function f6(): void {
    // Fine since we are typed void and return undefined
    return undefined;
}

function f7(): void {
    // Fine since we are typed void and return null
    return null; //~ ERROR: Type 'null' is not assignable to type 'void'.
}

function f8(): any {
    // Fine since are typed any.
    return;
}

function f9(): any {
    // Fine since we are typed any and return undefined
    return undefined;
}

function f10(): void {
    // Fine since we are typed any and return null
    return null; //~ ERROR: Type 'null' is not assignable to type 'void'.
}

function f11(): string {
    // Fine since we consist of a single throw statement.
    throw undefined;
}

function f12(): void {
    // Fine since we consist of a single throw statement.
    throw undefined;
}

function f13(): any {
    // Fine since we consist of a single throw statement.
    throw undefined;
}

function f14(): number {
    // Not fine, since we can *only* consist of a single throw statement
    // if no return statements are present but we are annotated.
    throw undefined;
    throw null;
}

function f15(): number {
    // Fine, since we have a return statement somewhere.
    throw undefined;
    throw null;
    return; //~ERROR: Type 'undefined' is not assignable to type 'number'.
}


function f16() {
    // Okay; not type annotated.
}

function f17() {
    // Okay; not type annotated.
    return;
}

function f18() {
    return "Okay, not type annotated.";
}

function f19(): void | number {
    // Okay; function return type is union containing void
}

function f20(): any | number {
    // Okay; function return type is union containing any
}

function f21(): number | string { //~ERROR: A function whose declared type is neither 'undefined', 'void', nor 'any' must return a value.
    // Not okay; union does not contain void or any
}

function f22(): undefined {
    // Okay; return type allows implicit return of undefined
}

function f23(): undefined | number {  //~ERROR: A function whose declared type is neither 'undefined', 'void', nor 'any' must return a value.
    // Error; because `undefined | number` becomes `number` without strictNullChecks.
}

const f30: () => undefined = () => {
    // Ok, contextual type for implicit return is `undefined`
}

const f31: () => undefined = () => {
    // Ok, contextual type for expression-less return is `undefined`
    return;
}

const f32: () => undefined | number = () => {
    // Error, contextual type for implicit return isn't just `undefined`
}

const f33: () => undefined | number = () => {
    // Error, contextual type for expression-less return isn't just `undefined`
    return;
}

class C {
    public get m1() { //~ERROR: A 'get' accessor must return a value.
        // Errors; get accessors must return a value.
    }

    public get m2() {
        // Permissible; returns undefined.
        return;
    }

    public get m3() {
        return "Okay, because this is a return expression.";
    }

    public get m4() {
        // Fine since this consists of a single throw statement.
        throw null;
    }

    public get m5() {
        // Not fine, since we can *only* consist of a single throw statement
        // if no return statements are present but we are a get accessor.
        throw null;
        throw undefined;
        throw undefined.
    } //~ERROR: Identifier expected.
}

