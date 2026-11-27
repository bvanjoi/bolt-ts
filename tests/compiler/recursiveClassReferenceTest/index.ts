// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/recursiveClassReferenceTest.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: lib=[es5]
//@compiler-options: sourcemap

// Scenario 1: Test reqursive function call with "this" parameter
// Scenario 2: Test recursive function call with cast and "this" parameter

declare namespace Sample.Thing {

	export interface IWidget {
		getDomNode(): any;
		destroy();
		gar(runner:(widget:Sample.Thing.IWidget)=>any):any;
	}

	export interface ICodeThing {
  
  		getDomNode(): Element;
      //~^ ERROR: Cannot find name 'Element'.
		
		addWidget(widgetId:string, widget:IWidget);

		
		focus(); 
		
		//addWidget(widget: Sample.Thing.Widgets.IWidget);
	}

	export interface IAction {
		run(Thing:ICodeThing):boolean;
		getId():string;
	}	
}

namespace Sample.Actions.Thing.Find {
	export class StartFindAction implements Sample.Thing.IAction {
		
		public getId() { return "yo"; }
		
		public run(Thing:Sample.Thing.ICodeThing):boolean {

			return true;
		}
	}
}

namespace Sample.Thing.Widgets {
	export class FindWidget implements Sample.Thing.IWidget {

		public gar(runner:(widget:Sample.Thing.IWidget)=>any) { if (true) {return runner(this);}}
			
		private domNode:any = null;
		constructor(private codeThing: Sample.Thing.ICodeThing) {
		    // scenario 1
		    codeThing.addWidget("addWidget", this);
		}
		
		public getDomNode() {
			return domNode;
      //~^ ERROR: Cannot find name 'domNode'.
		}
		
		public destroy() {

		}

	}
}

interface IMode { getInitialState(): IState;} 
class AbstractMode implements IMode { public getInitialState(): IState { return null;} }

interface IState {}

interface Window {
    opener: Window;
}
declare var self: Window;

namespace Sample.Thing.Languages.PlainText {
	
	export class State implements IState {		
        constructor(private mode: IMode) { }
		public clone():IState {
			return this;
		}

		public equals(other:IState):boolean {
			return this === other;
		}
		
		public getMode(): IMode { return mode; }
    //~^ ERROR: Cannot find name 'mode'.
	}
	
	export class Mode extends AbstractMode {

		// scenario 2
		public getInitialState(): IState {
			return new State(self);
      //~^ ERROR: Property 'getInitialState' is missing.
		}


	}
}

