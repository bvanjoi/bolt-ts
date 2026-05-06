// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/conditionalEqualityOnLiteralObjects.ts`, Apache-2.0 License

//@compiler-options: target=es2015

const a = { a: 1 }
const b = [1]

if ({ a: 1 } === { a: 1 }) {
//~^ ERROR: This condition will always return 'false' since JavaScript compares objects by reference, not value.
}
if ([1] === [1]) {
//~^ ERROR: This condition will always return 'false' since JavaScript compares objects by reference, not value.
}
if (a === { a: 1 }) {
//~^ ERROR: This condition will always return 'false' since JavaScript compares objects by reference, not value.
}
if (b === [1]) {
//~^ ERROR: This condition will always return 'false' since JavaScript compares objects by reference, not value.
}
if ({ a: 1 } === a) {
//~^ ERROR: This condition will always return 'false' since JavaScript compares objects by reference, not value.
}
if ([1] === b) {
//~^ ERROR: This condition will always return 'false' since JavaScript compares objects by reference, not value.
}

if ({ a: 1 } !== { a: 1 }) {
//~^ ERROR: This condition will always return 'true' since JavaScript compares objects by reference, not value.
}
if ([1] !== [1]) {
//~^ ERROR: This condition will always return 'true' since JavaScript compares objects by reference, not value.
}
if (a !== { a: 1 }) {
//~^ ERROR: This condition will always return 'true' since JavaScript compares objects by reference, not value.
}
if (b !== [1]) {
//~^ ERROR: This condition will always return 'true' since JavaScript compares objects by reference, not value.
}
if ({ a: 1 } !== a) {
//~^ ERROR: This condition will always return 'true' since JavaScript compares objects by reference, not value.
}
if ([1] !== b) {
//~^ ERROR: This condition will always return 'true' since JavaScript compares objects by reference, not value.
}

if ({ a: 1 } == { a: 1 }) {
//~^ ERROR: This condition will always return 'false' since JavaScript compares objects by reference, not value.
}
if ([1] == [1]) {
//~^ ERROR: This condition will always return 'false' since JavaScript compares objects by reference, not value.
}
if (a == { a: 1 }) {
//~^ ERROR: This condition will always return 'false' since JavaScript compares objects by reference, not value.
}
if (b == [1]) {
//~^ ERROR: This condition will always return 'false' since JavaScript compares objects by reference, not value.
}
if ({ a: 1 } == a) {
//~^ ERROR: This condition will always return 'false' since JavaScript compares objects by reference, not value.
}
if ([1] == b) {
//~^ ERROR: This condition will always return 'false' since JavaScript compares objects by reference, not value.
}

if ({ a: 1 } != { a: 1 }) {
//~^ ERROR: This condition will always return 'true' since JavaScript compares objects by reference, not value.
}
if ([1] != [1]) {
//~^ ERROR: This condition will always return 'true' since JavaScript compares objects by reference, not value.
}
if (a != { a: 1 }) {
//~^ ERROR: This condition will always return 'true' since JavaScript compares objects by reference, not value.
}
if (b != [1]) {
//~^ ERROR: This condition will always return 'true' since JavaScript compares objects by reference, not value.
}
if ({ a: 1 } != a) {
//~^ ERROR: This condition will always return 'true' since JavaScript compares objects by reference, not value.
}
if ([1] != b) {
//~^ ERROR: This condition will always return 'true' since JavaScript compares objects by reference, not value.
}
