export {x, x as y};
//~^ ERROR: Cannot export 'x'. Only local declarations can be exported from a module.
//~| ERROR: Cannot export 'x'. Only local declarations can be exported from a module.
export {x1, x1 as y1};
//~^ ERROR: Cannot export 'x1'. Only local declarations can be exported from a module.
//~| ERROR: Cannot export 'x1'. Only local declarations can be exported from a module.
export {a, a as a1};
//~^ ERROR: Cannot export 'a'. Only local declarations can be exported from a module.
//~| ERROR: Cannot export 'a'. Only local declarations can be exported from a module.
export {b, b as b1};
//~^ ERROR: Cannot export 'b'. Only local declarations can be exported from a module.
//~| ERROR: Cannot export 'b'. Only local declarations can be exported from a module.


export {x as z};
//~^ ERROR: Cannot export 'x'. Only local declarations can be exported from a module.
export {x1 as z1};
//~^ ERROR: Cannot export 'x1'. Only local declarations can be exported from a module.
export {a as a2};
//~^ ERROR: Cannot export 'a'. Only local declarations can be exported from a module.
export {b as b2};
//~^ ERROR: Cannot export 'b'. Only local declarations can be exported from a module.
