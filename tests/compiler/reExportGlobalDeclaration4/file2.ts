export {Cls1, Cls1 as CCls1};
//~^ ERROR: Cannot export 'Cls1'. Only local declarations can be exported from a module.
//~| ERROR: Cannot export 'Cls1'. Only local declarations can be exported from a module.
export {Cls2, Cls2 as CCls2};
//~^ ERROR: Cannot export 'Cls2'. Only local declarations can be exported from a module.
//~| ERROR: Cannot export 'Cls2'. Only local declarations can be exported from a module.
export {Cls1 as CCCls1};
//~^ ERROR: Cannot export 'Cls1'. Only local declarations can be exported from a module.
export {Cls2 as CCCls2};
//~^ ERROR: Cannot export 'Cls2'. Only local declarations can be exported from a module.
