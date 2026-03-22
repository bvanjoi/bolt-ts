// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/arrowFunctionParsingDoesNotConfuseParenthesizedObjectForArrowHead.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare var value: boolean;
declare var a: any;

const test = () => ({
    // "Identifier expected." error on "!" and two "Duplicate identifier '(Missing)'." errors on space.
    prop: !value, // remove ! to see that errors will be gone
    run: () => { //replace arrow function with regular function to see that errors will be gone
        // comment next line or remove "()" to see that errors will be gone
        if(!a.b()) { return 'special'; }

        return 'default';
    }
}); 
