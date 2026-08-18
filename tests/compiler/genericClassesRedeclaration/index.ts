// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericClassesRedeclaration.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare namespace TypeScript {
    interface IIndexable<T> {
        [s: string]: T;
        //~^ ERROR: Duplicate index signature for type 'string'.
    }
    function createIntrinsicsObject<T>(): IIndexable<T>;
    interface IHashTable<T> {
        getAllKeys(): string[];
        add(key: string, data: T): boolean;
        addOrUpdate(key: string, data: T): boolean;
        map(fn: (k: string, value: T, context: any) => void, context: any): void;
        every(fn: (k: string, value: T, context: any) => void, context: any): boolean;
        some(fn: (k: string, value: T, context: any) => void, context: any): boolean;
        count(): number;
        lookup(key: string): T;
    }
    class StringHashTable<T> implements IHashTable<T> {
        private itemCount;
        private table;
        public getAllKeys(): string[];
        public add(key: string, data: T): boolean;
        public addOrUpdate(key: string, data: T): boolean;
        public map(fn: (k: string, value: T, context: any) => void, context: any): void;
        public every(fn: (k: string, value: T, context: any) => void, context: any): boolean;
        public some(fn: (k: string, value: T, context: any) => void, context: any): boolean;
        public count(): number;
        public lookup(key: string): T;
        public remove(key: string): void;
    }
    class IdentifierNameHashTable<T> extends StringHashTable<T> {
        public getAllKeys(): string[];
        public add(key: string, data: T): boolean;
        public addOrUpdate(key: string, data: T): boolean;
        public map(fn: (k: string, value: T, context: any) => void, context: any): void;
        public every(fn: (k: string, value: T, context: any) => void, context: any): boolean;
        public some(fn: (k: string, value: any, context: any) => void, context: any): boolean;
        public lookup(key: string): T;
    }
}

declare namespace TypeScript {
    interface IIndexable<T> {
        [s: string]: T;
        //~^ ERROR: Duplicate index signature for type 'string'.
    }
    function createIntrinsicsObject<T>(): IIndexable<T>;
    interface IHashTable<T> {
        getAllKeys(): string[];
        add(key: string, data: T): boolean;
        addOrUpdate(key: string, data: T): boolean;
        map(fn: (k: string, value: T, context: any) => void, context: any): void;
        every(fn: (k: string, value: T, context: any) => void, context: any): boolean;
        some(fn: (k: string, value: T, context: any) => void, context: any): boolean;
        count(): number;
        lookup(key: string): T;
    }
    class StringHashTable<T> implements IHashTable<T> {
      //~^ ERROR: Duplicate identifier 'StringHashTable'.
        private itemCount;
        private table;
        public getAllKeys(): string[];
        public add(key: string, data: T): boolean;
        public addOrUpdate(key: string, data: T): boolean;
        public map(fn: (k: string, value: T, context: any) => void, context: any): void;
        public every(fn: (k: string, value: T, context: any) => void, context: any): boolean;
        public some(fn: (k: string, value: T, context: any) => void, context: any): boolean;
        public count(): number;
        public lookup(key: string): T;
        public remove(key: string): void;
    }
    class IdentifierNameHashTable<T> extends StringHashTable<T> {
      //~^ ERROR: Duplicate identifier 'IdentifierNameHashTable'.
        public getAllKeys(): string[];
        public add(key: string, data: T): boolean;
        public addOrUpdate(key: string, data: T): boolean;
        public map(fn: (k: string, value: T, context: any) => void, context: any): void;
        public every(fn: (k: string, value: T, context: any) => void, context: any): boolean;
        public some(fn: (k: string, value: any, context: any) => void, context: any): boolean;
        public lookup(key: string): T;
    }
}