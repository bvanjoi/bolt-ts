type undefined = string; //~ ERROR: Type alias name cannot be 'undefined'.
function p(undefined = "wat") {
	return undefined;
}
