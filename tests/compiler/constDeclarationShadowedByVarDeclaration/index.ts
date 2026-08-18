// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/constDeclarationShadowedByVarDeclaration.ts`, Apache-2.0 License

//@compiler-options: target=ES6

// Error as declaration of var would cause a write to the const value
var x = 0;
{
    const x = 0;

    var x = 0;
    //~^ ERROR: Cannot initialize outer scoped variable 'x' in the same scope as block scoped declaration 'x'.
}


var y = 0;
{
    const y = 0;
    {
        var y = 0;
    //~^ ERROR: Cannot initialize outer scoped variable 'y' in the same scope as block scoped declaration 'y'.
    }
}


{
  const z = 0;
  var z = 0
  //~^ ERROR: Cannot initialize outer scoped variable 'z' in the same scope as block scoped declaration 'z'.
}