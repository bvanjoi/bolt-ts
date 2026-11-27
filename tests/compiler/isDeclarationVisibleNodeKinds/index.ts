// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/isDeclarationVisibleNodeKinds.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
//@compiler-options: declaration

// Function types
namespace schema {
    export function createValidator1(schema: any): <T>(data: T) => T {
        return undefined;
        //~^ ERROR: Type 'undefined' is not assignable to type '<T>(data: T) => T'.
    }
}

// Constructor types
namespace schema {
    export function createValidator2(schema: any): new <T>(data: T) => T {
        return undefined;
        //~^ ERROR: Type 'undefined' is not assignable to type 'new <T>(data: T) => T'.
    }
}

// union types
namespace schema {
     export function createValidator3(schema: any): number | { new <T>(data: T): T; }  {
        return undefined;
        //~^ ERROR: Type 'undefined' is not assignable to type 'number | (new <T>(data: T) => T)'.
    }
}

// Array types
namespace schema {
     export function createValidator4(schema: any): { new <T>(data: T): T; }[] {
        return undefined;
        //~^ ERROR: Type 'undefined' is not assignable to type 'new <T>(data: T) => T[]'.
    }
}


// TypeLiterals
namespace schema {
    export function createValidator5(schema: any): { new <T>(data: T): T } {
        return undefined;
        //~^ ERROR: Type 'undefined' is not assignable to type 'new <T>(data: T) => T'.
    }
}

// Tuple types
namespace schema {
    export function createValidator6(schema: any): [ new <T>(data: T) => T, number] {
        return undefined;
        //~^ ERROR: Type 'undefined' is not assignable to type '[new <T>(data: T) => T, number]'.
    }
}

// Paren Types
namespace schema {
    export function createValidator7(schema: any): (new <T>(data: T)=>T )[] {
        return undefined;
        //~^ ERROR: Type 'undefined' is not assignable to type 'new <T>(data: T) => T[]'.
    }
}

// Type reference
namespace schema {
    export function createValidator8(schema: any): Array<{ <T>(data: T) : T}> {
        return undefined;
        //~^ ERROR: Type 'undefined' is not assignable to type '<T>(data: T) => T[]'.
    }
}


namespace schema {
    export class T {
        get createValidator9(): <T>(data: T) => T {
            return undefined;
            //~^ ERROR: Type 'undefined' is not assignable to type '<T>(data: T) => T'.
        }
        
        set createValidator10(v: <T>(data: T) => T) {
        }
    }
}
