// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/externModuleClobber.ts`, Apache-2.0 License

declare module EM {
	export class Position { }

	export class EC {
		public getPosition() : EM.Position;
	}
}

var x:EM.Position;
var ec:EM.EC = new EM.EC();

x = ec.getPosition();

var y: string = ec.getPosition();
//~^ ERROR: Type 'Position' is not assignable to type 'string'.