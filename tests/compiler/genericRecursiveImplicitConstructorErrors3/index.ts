// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericRecursiveImplicitConstructorErrors3.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace TypeScript {
    export class MemberName <A,B,C>{
        static create<A,B,C>(arg1: any, arg2?: any, arg3?: any): MemberName {
      //~^ ERROR: Generic type 'TypeScript.MemberName<A, B, C>' requires 3 type arguments.
        }
    }
}
 
namespace TypeScript {
    export class PullSymbol <A,B,C>{
        public type: PullTypeSymbol = null;
      //~^ ERROR: Generic type 'TypeScript.PullTypeSymbol<A, B, C>' requires 3 type arguments.
    }
    export class PullTypeSymbol <A,B,C>extends PullSymbol {
      //~^ ERROR: Generic type 'TypeScript.PullSymbol<A, B, C>' requires 3 type arguments.
        private _elementType: PullTypeSymbol = null;
          //~^ ERROR: Generic type 'TypeScript.PullTypeSymbol<A, B, C>' requires 3 type arguments.
        public toString<A,B,C>(scopeSymbol?: PullSymbol, useConstraintInName?: boolean) {
          //~^ ERROR: Generic type 'TypeScript.PullSymbol<A, B, C>' requires 3 type arguments.
            var s = this.getScopedNameEx(scopeSymbol, useConstraintInName).toString();
            return s;
        }
        public getScopedNameEx<A,B,C>(scopeSymbol?: PullSymbol, useConstraintInName?: boolean, getPrettyTypeName?: boolean, getTypeParamMarkerInfo?: boolean) {
          //~^ ERROR: Generic type 'TypeScript.PullSymbol<A, B, C>' requires 3 type arguments.
            if (this.isArray()) {
              //~^ ERROR: Property 'isArray' does not exist on type 'TypeScript.PullTypeSymbol<A, B, C, PullTypeSymbol>'.
                var elementMemberName = this._elementType ?
                (this._elementType.isArray() || this._elementType.isNamedTypeSymbol() ?
                this._elementType.getScopedNameEx(scopeSymbol, false, getPrettyTypeName, getTypeParamMarkerInfo) :
                this._elementType.getMemberTypeNameEx(false, scopeSymbol, getPrettyTypeName)) : 1
                return MemberName.create(elementMemberName, "", "[]");
            }
        }
    }
}