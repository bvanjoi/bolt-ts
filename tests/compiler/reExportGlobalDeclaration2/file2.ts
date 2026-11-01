export {I1, I1 as II1};
//~^ ERROR: Cannot export 'I1'. Only local declarations can be exported from a module.
//~| ERROR: Cannot export 'I1'. Only local declarations can be exported from a module.
export {I2, I2 as II2};
//~^ ERROR: Cannot export 'I2'. Only local declarations can be exported from a module.
//~| ERROR: Cannot export 'I2'. Only local declarations can be exported from a module.
export {I1 as III1};
//~^ ERROR: Cannot export 'I1'. Only local declarations can be exported from a module.
export {I2 as III2};
//~^ ERROR: Cannot export 'I2'. Only local declarations can be exported from a module.
