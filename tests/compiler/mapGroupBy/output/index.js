var basic = Map.groupBy([0, 2, 8], (x) => (x < 5 ? 'small' : 'large'));
var chars = Map.groupBy('a string', (c) => (c));
var employees = new Set();
var byRole = Map.groupBy(employees, (x) => (x.role));
var byNonKey = Map.groupBy(employees, (x) => (x));