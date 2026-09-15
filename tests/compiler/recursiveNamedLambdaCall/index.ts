// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/recursiveNamedLambdaCall.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: lib=[es5]

var promise = function( obj ) {
	
		if ( top && top.doScroll ) {
			//~^ ERROR: Cannot find name 'top'.
			//~| ERROR: Cannot find name 'top'.
			(function doScrollCheck() {
				if ( false ) {

					try {
						top.doScroll("left");
					//~^ ERROR: Cannot find name 'top'.
					} catch(e) {
						return setTimeout( doScrollCheck, 50 );
					//~^ ERROR: Cannot find name 'setTimeout'.
					}

					// detach all dom ready events
					detach();
					//~^ ERROR: Cannot find name 'detach'.
				}
			})();
		}
};
