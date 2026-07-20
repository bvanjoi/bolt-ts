// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/lambdaArgCrash.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: lib=[es5]

class Event {

	 private _listeners: any[] = [];

	 constructor () {

		 // TODO: remove

		 this._listeners = [];

	}

	 add(listener: () => any): void {

		 /// <summary>Registers a new listener for the event.</summary>

		 /// <param name="listener">The callback function to register.</param>

		 this._listeners.push(listener);

	}

}
 
class ItemSetEvent extends Event {

	 add(listener: (items: ItemSet) => void ) {
									//~^ ERROR: Cannot find name 'ItemSet'.
	 	super.add(listener);
		//~^ ERROR: Argument of type '(items: error) => void' is not assignable to parameter of type '() => any'.

	}

}

