// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/shorthandPropertyAssignmentsInDestructuring_ES6.ts`, Apache-2.0 License

//@compiler-options: target=ES6


(function() {
	var s0;
	for ({ s0 = 5 } of [{ s0: 1 }]) {
	}
});
(function() {
	var s0;
	for ({ s0:s0 = 5 } of [{ s0: 1 }]) {
	}
});

(function() {
	var s1;
	for ({ s1 = 5 } of [{}]) {
	}
});

(function() {
	var s1;
	for ({ s1:s1 = 5 } of [{}]) {
	}
});

(function() {
	var s2;
	for ({ s2 = 5 } of [{ s2: "" }]) {
	}
});

(function() {
	var s2;
	for ({ s2:s2 = 5 } of [{ s2: "" }]) {
	}
});

(function() {
	var s3: string;
	for ({ s3 = 5 } of [{ s3: "" }]) {
    //~^ ERROR: Type 'number' is not assignable to type 'string'.
	}
});

(function() {
	var s3: string;
	for ({ s3:s3 = 5 } of [{ s3: "" }]) {
    //~^ ERROR: Type 'number' is not assignable to type 'string'.
	}
});

(function() {
	let y;
	({ y = 5 } = { y: 1 })
});

(function() {
	let y;
	({ y:y = 5 } = { y: 1 })
});

(function() {
	let y0: number;
	({ y0 = 5 } = { y0: 1 })
});

(function() {
	let y0: number;
	({ y0:y0 = 5 } = { y0: 1 })
});

(function() {
	let y1: string;
	({ y1 = 5 } = {})
	//~^ ERROR: Type 'number' is not assignable to type 'string'.
});

(function() {
	let y1: string;
	({ y1:y1 = 5 } = {})
	//~^ ERROR: Type 'number' is not assignable to type 'string'.
	//~| ERROR: Type 'number' is not assignable to type 'string'.
});

(function() {
	let y2: string, y3: { x: string };
	({ y2 = 5, y3 = { x: 1 } } = {})
	//~^ ERROR: Type 'number' is not assignable to type 'string'.
	//~| ERROR: Type 'number' is not assignable to type 'string'.
});

(function() {
	let y2: string, y3: { x: string };
	({ y2:y2 = 5, y3:y3 = { x: 1 } } = {})
	//~^ ERROR: Type 'number' is not assignable to type 'string'.
	//~| ERROR: Type 'number' is not assignable to type 'string'.
	//~| ERROR: Type 'number' is not assignable to type 'string'.
	//~| ERROR: Type 'number' is not assignable to type 'string'.
});

(function() {
	let y4: number, y5: { x: number };
	({ y4 = 5, y5 = { x: 1 } } = {})
});

(function() {
	let y4: number, y5: { x: number };
	({ y4:y4 = 5, y5:y5 = { x: 1 } } = {})
});


(function() {
	let z;
	({ z = { x: 5 } } = { z: { x: 1 } });
});


(function() {
	let z;
	({ z:z = { x: 5 } } = { z: { x: 1 } });
});

(function() {
	let a = { s = 5 };
	//~^ ERROR: Cannot find name 's'.
	//~| ERROR: Did you mean to use a ':'? An '=' can only follow a property name when the containing object literal is part of a destructuring pattern.
});

function foo({a = 4, b = { x: 5 }}) {
}