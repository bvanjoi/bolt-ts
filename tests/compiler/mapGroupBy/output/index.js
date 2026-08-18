// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/mapGroupBy.ts`, Apache-2.0 License
var basic = Map.groupBy([0, 2, 8], (x) => (x < 5 ? 'small' : 'large'));
var chars = Map.groupBy('a string', (c) => (c));
var employees = new Set();
var byRole = Map.groupBy(employees, (x) => (x.role));
var byNonKey = Map.groupBy(employees, (x) => (x));