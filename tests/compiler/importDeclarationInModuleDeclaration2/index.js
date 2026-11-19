// @target: es2015
// @allowJs: true
// @noEmit: true
// @checkJs: true

function container() {
    import "fs";
    //~^ ERROR: An import declaration can only be used at the top level of a module.
}