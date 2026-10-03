// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/listFailure.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace Editor {

    export class Buffer {
    	lines: List<Line> = ListMakeHead<Line>();
        
        addLine(lineText: string): List<Line> {
            
            var line: Line = new Line();
            var lineEntry = this.lines.add(line);

            return lineEntry;
        }    
    }
    
    export function ListRemoveEntry<U>(entry: List<U>): List<U> { 
    	return entry;
    }

    export function ListMakeHead<U>(): List<U> {
		return null;
    //~^ ERROR: Type 'null' is not assignable to type 'List<U>'.
    }

    export function ListMakeEntry<U>(data: U): List<U> {
		return null;
    //~^ ERROR: Type 'null' is not assignable to type 'List<U>'.
    }    

    class List<T> { 
        public next: List<T>; 
        //~^ ERROR: Property 'next' has no initializer and is not definitely assigned in the constructor.

        add(data: T): List<T> {
            this.next = ListMakeEntry(data);
            return this.next;
        }

        popEntry(head: List<T>): List<T> {
            return (ListRemoveEntry(this.next));
        }      
    }

	export class Line {}
}