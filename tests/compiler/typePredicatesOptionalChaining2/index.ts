// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typePredicatesOptionalChaining2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

type Person = { name: string; }

const getName1 = (person?: Person): string => {
  return typeof person?.name === 'string' ? person?.name : '';
};

const isString = (value: any): value is string => {
  return typeof value === 'string';
};

const getName2 = (person?: Person): string => {
  return isString(person?.name) ? person?.name : '';
};

const getName0 = (person?: Person): string => {
  return typeof person?.name === 'string' ? person.name : '';
};

const getName3 = (person?: Person): string => {
  return isString(person?.name) ? person.name : '';
};