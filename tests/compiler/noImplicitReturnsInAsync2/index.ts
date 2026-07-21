// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noImplicitReturnsInAsync2.ts`, Apache-2.0 License

//@compiler-options: strict=false
//@compiler-options: target=es6
//@compiler-options: noImplicitReturns

// Should be an error, Promise<number>, currently retorted correctly 
async function test3(isError: boolean = true) {
  //~^ ERROR: Not all code paths return a value.
    if (isError === true) {
        return 6;
    }
}

// Should not be an error, Promise<any>, currently **not** working
async function test4(isError: boolean = true) {  
    if (isError === true) {
        return undefined;
    }
}

// should not be error, Promise<any> currently working correctly 
async function test5(isError: boolean = true): Promise<any> { //should not be error
    if (isError === true) {
        return undefined;
    }
}


// should be error, currently reported correctly 
async function test6(isError: boolean = true): Promise<number> { 
  //~^ ERROR: Not all code paths return a value.
    if (isError === true) {
        return undefined;
    }
}

// infered to be Promise<void>, should not be an error, currently reported correctly 
async function test7(isError: boolean = true) { 
    if (isError === true) {
        return;
    }
}