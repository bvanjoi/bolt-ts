// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/controlFlowManyConsecutiveConditionsNoTimeout.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

const a: string = 42
//~^ ERROR: Type 'number' is not assignable to type 'string'.

export enum Choice {
    One,
    Two,
}

const choice: Choice = Choice.One;
const choiceOne = Choice.One;

if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {}
if (choice === choiceOne) {} 

while (true) {
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {}
    if (choice === choiceOne) {} 
}

