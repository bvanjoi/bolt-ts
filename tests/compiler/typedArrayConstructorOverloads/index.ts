// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typedArrayConstructorOverloads.ts`, Apache-2.0 License

//@compiler-options: target=esnext
//@compiler-options: noEmit

// https://github.com/microsoft/TypeScript/issues/60367

type TypedArrayConstructor =
    | Int8ArrayConstructor
    | Uint8ArrayConstructor
    | Uint8ClampedArrayConstructor
    | Int16ArrayConstructor
    | Uint16ArrayConstructor
    | Int32ArrayConstructor
    | Uint32ArrayConstructor
    | Float16ArrayConstructor
    | Float32ArrayConstructor
    | Float64ArrayConstructor
    | BigInt64ArrayConstructor
    | BigUint64ArrayConstructor

export function makeTypedArray(buffer: ArrayBuffer, ctr: TypedArrayConstructor) {
    new ctr(buffer);
    new ctr(buffer, 0, 0);
}
