declare let a: any;

a?.b``;
//~^ ERROR: Tagged template expressions are not permitted in an optional chain.
a?.`b`;
//~^ ERROR: Tagged template expressions are not permitted in an optional chain.
a?.`b${1}c`;
//~^ ERROR: Tagged template expressions are not permitted in an optional chain.
