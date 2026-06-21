var getName1 = (person) => (typeof person.name === 'string' ? person.name : '');
var isString = (value) => (typeof value === 'string');
var getName2 = (person) => (isString(person.name) ? person.name : '');
var getName0 = (person) => (typeof person.name === 'string' ? person.name : '');
var getName3 = (person) => (isString(person.name) ? person.name : '');