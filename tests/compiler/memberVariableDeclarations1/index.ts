// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/memberVariableDeclarations1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class Employee {
    public name: string;
    //~^ ERROR: Property 'name' has no initializer and is not definitely assigned in the constructor.
    public address: string;
    //~^ ERROR: Property 'address' has no initializer and is not definitely assigned in the constructor.
    public retired = false;
    public manager: Employee = null;
    //~^ ERROR: Type 'null' is not assignable to type 'Employee'.
    public reports: Employee[] = [];
}

class Employee2 {
    public name: string;
    //~^ ERROR: Property 'name' has no initializer and is not definitely assigned in the constructor.
    public address: string;
    //~^ ERROR: Property 'address' has no initializer and is not definitely assigned in the constructor.
    public retired: boolean;
    public manager: Employee;
    public reports: Employee[];
    constructor() {
        this.retired = false;
        this.manager = null;
        //~^ ERROR: Type 'null' is not assignable to type 'Employee'.
        this.reports = [];
    }
}

var e1: Employee;
var e2: Employee2;
e1 = e2;
//~^ ERROR: Variable 'e2' is used before being assigned.
e2 = e1;