// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/moduleImport.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace A.B.C {
	import XYZ = X.Y.Z;
  //~^ ERROR: Namespace 'X' has no exported member 'Y'.
	export function ping(x: number) {
		if (x>0) XYZ.pong (x-1);
	}
}

namespace X {
	import ABC = A.B.C;
	export function pong(x: number) {
		if (x > 0) ABC.ping(x-1);
	}
}