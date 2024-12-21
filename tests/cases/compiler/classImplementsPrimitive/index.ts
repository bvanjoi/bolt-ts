// From `github.com/microsoft/TypeScript/blob/v5.7.2/tests/cases/compiler/classImplementsPrimitive.ts`, Apache-2.0 License

class C implements number { }
//~^ ERROR: A class cannot implement a primitive type like 'number'.
class C2 implements string { }
//~^ ERROR: A class cannot implement a primitive type like 'string'.
class C3 implements boolean { }
//~^ ERROR: A class cannot implement a primitive type like 'boolean'.

const C4 = class implements number {}
//~^ ERROR: A class cannot implement a primitive type like 'number'.
const C5 = class implements string {}
//~^ ERROR: A class cannot implement a primitive type like 'string'.
const C6 = class implements boolean {}
//~^ ERROR: A class cannot implement a primitive type like 'boolean'.

const C7 = class A implements number { }
//~^ ERROR: A class cannot implement a primitive type like 'number'.
const C8 = class B implements string { }
//~^ ERROR: A class cannot implement a primitive type like 'string'.
const C9 = class C implements boolean { }
//~^ ERROR: A class cannot implement a primitive type like 'boolean'.
