// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/genericArrayPropertyAssignment.ts`, Apache-2.0 License

function isEmpty(list: {length:number;})
{
return list.length ===0;
}
 
isEmpty([]); 
 
