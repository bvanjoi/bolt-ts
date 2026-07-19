// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/targetTypeObjectLiteralToAny.ts`, Apache-2.0 License

function suggest(){ 
	var TypeScriptKeywords:string[];  
	var result:any;	
					
	TypeScriptKeywords.forEach(function(keyword) {						
	//~^ ERROR: Variable 'TypeScriptKeywords' is used before being assigned.
		result.push({text:keyword, type:"keyword"}); // this should not cause a crash - push should be typed to any
	});			
}

