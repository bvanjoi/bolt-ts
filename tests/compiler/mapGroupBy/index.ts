// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/mapGroupBy.ts`, Apache-2.0 License

//@compiler-options: target=esnext

const basic = Map.groupBy([0, 2, 8], x => x < 5 ? 'small' : 'large');

const chars = Map.groupBy('a string', c => c);

type Employee = { name: string, role: 'ic' | 'manager' }
const employees: Set<Employee> = new Set();
const byRole = Map.groupBy(employees, x => x.role);

const byNonKey = Map.groupBy(employees, x => x);
