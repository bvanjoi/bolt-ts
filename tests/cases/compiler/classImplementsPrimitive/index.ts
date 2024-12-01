// classes cannot implement primitives

class C implements number { }
//~^ ERROR: A class cannot implement a primitive type like 'number'. It can only implement other named object types.
class C2 implements string { }
//~^ ERROR: A class cannot implement a primitive type like 'string'. It can only implement other named object types.
class C3 implements boolean { }
//~^ ERROR: A class cannot implement a primitive type like 'boolean'. It can only implement other named object types.

const C4 = class implements number {}
//~^ ERROR: A class cannot implement a primitive type like 'number'. It can only implement other named object types.
const C5 = class implements string {}
//~^ ERROR: A class cannot implement a primitive type like 'string'. It can only implement other named object types.
const C6 = class implements boolean {}
//~^ ERROR: A class cannot implement a primitive type like 'boolean'. It can only implement other named object types.

const C7 = class A implements number { }
//~^ ERROR: A class cannot implement a primitive type like 'number'. It can only implement other named object types.
const C8 = class B implements string { }
//~^ ERROR: A class cannot implement a primitive type like 'string'. It can only implement other named object types.
const C9 = class C implements boolean { }
//~^ ERROR: A class cannot implement a primitive type like 'boolean'. It can only implement other named object types.
