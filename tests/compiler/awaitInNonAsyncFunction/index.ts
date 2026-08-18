// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/awaitInNonAsyncFunction.ts`, Apache-2.0 License

//@compiler-options: strict=false
//@compiler-options: target=esnext

// https://github.com/Microsoft/TypeScript/issues/26586

function normalFunc(p: Promise<number>) {
  for await (const _ of []);
  //~^ ERROR: 'for await' loops are only allowed within async functions and at the top levels of modules.
  return await p;
  //~^ ERROR: 'await' expressions are only allowed within async functions and at the top levels of modules.
}

export function exportedFunc(p: Promise<number>) {
  for await (const _ of []);
  //~^ ERROR: 'for await' loops are only allowed within async functions and at the top levels of modules.
  return await p;
  //~^ ERROR: 'await' expressions are only allowed within async functions and at the top levels of modules.
}

const functionExpression = function(p: Promise<number>) {
  for await (const _ of []);
  //~^ ERROR: 'for await' loops are only allowed within async functions and at the top levels of modules.
  await p;
  //~^ ERROR: 'await' expressions are only allowed within async functions and at the top levels of modules.
}

const arrowFunc = (p: Promise<number>) => {
  for await (const _ of []);
  //~^ ERROR: 'for await' loops are only allowed within async functions and at the top levels of modules.
  return await p;
  //~^ ERROR: 'await' expressions are only allowed within async functions and at the top levels of modules.
};

function* generatorFunc(p: Promise<number>) {
  for await (const _ of []);
  //~^ ERROR: 'for await' loops are only allowed within async functions and at the top levels of modules.
  yield await p;
  //~^ ERROR: 'await' expressions are only allowed within async functions and at the top levels of modules.
}

class clazz {
  constructor(p: Promise<number>) {
    for await (const _ of []);
  //~^ ERROR: 'for await' loops are only allowed within async functions and at the top levels of modules.
    await p;
  //~^ ERROR: 'await' expressions are only allowed within async functions and at the top levels of modules.
  }
  method(p: Promise<number>) {
  for await (const _ of []);
  //~^ ERROR: 'for await' loops are only allowed within async functions and at the top levels of modules.
    await p;
  //~^ ERROR: 'await' expressions are only allowed within async functions and at the top levels of modules.
  }
}

for await (const _ of []);
await null;