// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/objectGroupBy.ts`, Apache-2.0 License

//@compiler-options: target=esnext

const basic = Object.groupBy([0, 2, 8], x => x < 5 ? 'small' : 'large');

const chars = Object.groupBy('a string', c => c);

type Employee = { name: string, role: 'ic' | 'manager' }
const employees: Set<Employee> = new Set();
const byRole = Object.groupBy(employees, x => x.role);

const byNonKey = Object.groupBy(employees, x => x);
//~^ ERROR: Type 'Employee' is not assignable to type 'symbol | number | string'.
