export {NS1, NS1 as NNS1};
//~^ ERROR: Cannot export 'NS1'. Only local declarations can be exported from a module.
//~| ERROR: Cannot export 'NS1'. Only local declarations can be exported from a module.
export {NS2, NS2 as NNS2};
//~^ ERROR: Cannot export 'NS2'. Only local declarations can be exported from a module.
//~| ERROR: Cannot export 'NS2'. Only local declarations can be exported from a module.
export {NS1 as NNNS1};
//~^ ERROR: Cannot export 'NS1'. Only local declarations can be exported from a module.
export {NS2 as NNNS2};
//~^ ERROR: Cannot export 'NS2'. Only local declarations can be exported from a module.