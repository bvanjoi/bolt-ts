// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/bestCommonTypeWithContextualTyping.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

interface Contextual {
    dummy;
    p?: number;
}

interface Ellement {
    dummy;
    p: any;
}

declare var e: Ellement;

// All of these should pass. Neither type is a supertype of the other, but the RHS should
// always use Ellement in these examples (not Contextual). Because Ellement is assignable
// to Contextual, no errors.
var arr: Contextual[] = [e]; // Ellement[]
var obj: { [s: string]: Contextual } = { s: e }; // { s: Ellement; [s: string]: Ellement }

var conditional: Contextual = null ? e : e; // Ellement
//~^ ERROR: This kind of expression is always falsy.
var contextualOr: Contextual = e || e; // Ellement
