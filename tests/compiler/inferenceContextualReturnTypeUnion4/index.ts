// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inferenceContextualReturnTypeUnion4.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit
//@compiler-options: lib=[esnext]

// https://github.com/microsoft/TypeScript/issues/57905

export abstract class Storage {
    abstract get<T extends string>(): T | Promise<T>;
}

export abstract class SyncStorage extends Storage {
    abstract override get<T extends string>(): T;
}

export abstract class ASyncStorage extends Storage {
    abstract override get<T extends string>(): Promise<T>;
}
