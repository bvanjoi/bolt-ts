var foo: {id:number;} = {id:4}; foo = {id: 5, name:"foo"};
//~^ ERROR: Object literal may only specify known properties, and 'name' does not exist.