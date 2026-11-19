export { X };
//~^ ERROR: Cannot export 'X'. Only local declarations can be exported from a module.
export declare function foo(): X.bar;