// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typedArraysCrossAssignability01.ts`, Apache-2.0 License

//@compiler-options: target=es6

function CheckAssignability() {
    let arr_Int8Array           = new Int8Array(1);
    let arr_Uint8Array          = new Uint8Array(1);
    let arr_Int16Array          = new Int16Array(1);
    let arr_Uint16Array         = new Uint16Array(1);
    let arr_Int32Array          = new Int32Array(1);
    let arr_Uint32Array         = new Uint32Array(1);
    let arr_Float32Array        = new Float32Array(1);
    let arr_Float64Array        = new Float64Array(1);
    let arr_Uint8ClampedArray   = new Uint8ClampedArray(1);

    arr_Int8Array = arr_Int8Array;
    arr_Int8Array = arr_Uint8Array;
    //~^ ERROR: Type 'Uint8Array<ArrayBuffer>' is not assignable to type 'Int8Array<ArrayBuffer>'.
    arr_Int8Array = arr_Int16Array;
    //~^ ERROR: Type 'Int16Array<ArrayBuffer>' is not assignable to type 'Int8Array<ArrayBuffer>'.
    arr_Int8Array = arr_Uint16Array;
    //~^ ERROR: Type 'Uint16Array<ArrayBuffer>' is not assignable to type 'Int8Array<ArrayBuffer>'.
    arr_Int8Array = arr_Int32Array;
    //~^ ERROR: Type 'Int32Array<ArrayBuffer>' is not assignable to type 'Int8Array<ArrayBuffer>'.
    arr_Int8Array = arr_Uint32Array;
    //~^ ERROR: Type 'Uint32Array<ArrayBuffer>' is not assignable to type 'Int8Array<ArrayBuffer>'.
    arr_Int8Array = arr_Float32Array;
    //~^ ERROR: Type 'Float32Array<ArrayBuffer>' is not assignable to type 'Int8Array<ArrayBuffer>'.
    arr_Int8Array = arr_Float64Array;
    //~^ ERROR: Type 'Float64Array<ArrayBuffer>' is not assignable to type 'Int8Array<ArrayBuffer>'.
    arr_Int8Array = arr_Uint8ClampedArray;
    //~^ ERROR: Type 'Uint8ClampedArray<ArrayBuffer>' is not assignable to type 'Int8Array<ArrayBuffer>'.

    arr_Uint8Array = arr_Int8Array;
    //~^ ERROR: Type 'Int8Array<ArrayBuffer>' is not assignable to type 'Uint8Array<ArrayBuffer>'.
    arr_Uint8Array = arr_Uint8Array;
    arr_Uint8Array = arr_Int16Array;
    //~^ ERROR: Type 'Int16Array<ArrayBuffer>' is not assignable to type 'Uint8Array<ArrayBuffer>'.
    arr_Uint8Array = arr_Uint16Array;
    //~^ ERROR: Type 'Uint16Array<ArrayBuffer>' is not assignable to type 'Uint8Array<ArrayBuffer>'.
    arr_Uint8Array = arr_Int32Array;
    //~^ ERROR: Type 'Int32Array<ArrayBuffer>' is not assignable to type 'Uint8Array<ArrayBuffer>'.
    arr_Uint8Array = arr_Uint32Array;
    //~^ ERROR: Type 'Uint32Array<ArrayBuffer>' is not assignable to type 'Uint8Array<ArrayBuffer>'.
    arr_Uint8Array = arr_Float32Array;
    //~^ ERROR: Type 'Float32Array<ArrayBuffer>' is not assignable to type 'Uint8Array<ArrayBuffer>'.
    arr_Uint8Array = arr_Float64Array;
    //~^ ERROR: Type 'Float64Array<ArrayBuffer>' is not assignable to type 'Uint8Array<ArrayBuffer>'.
    arr_Uint8Array = arr_Uint8ClampedArray;
    //~^ ERROR: Type 'Uint8ClampedArray<ArrayBuffer>' is not assignable to type 'Uint8Array<ArrayBuffer>'.

    arr_Int16Array = arr_Int8Array;
    //~^ ERROR: Type 'Int8Array<ArrayBuffer>' is not assignable to type 'Int16Array<ArrayBuffer>'.
    arr_Int16Array = arr_Uint8Array;
    //~^ ERROR: Type 'Uint8Array<ArrayBuffer>' is not assignable to type 'Int16Array<ArrayBuffer>'.
    arr_Int16Array = arr_Int16Array;
    arr_Int16Array = arr_Uint16Array;
    //~^ ERROR: Type 'Uint16Array<ArrayBuffer>' is not assignable to type 'Int16Array<ArrayBuffer>'.
    arr_Int16Array = arr_Int32Array;
    //~^ ERROR: Type 'Int32Array<ArrayBuffer>' is not assignable to type 'Int16Array<ArrayBuffer>'.
    arr_Int16Array = arr_Uint32Array;
    //~^ ERROR: Type 'Uint32Array<ArrayBuffer>' is not assignable to type 'Int16Array<ArrayBuffer>'.
    arr_Int16Array = arr_Float32Array;
    //~^ ERROR: Type 'Float32Array<ArrayBuffer>' is not assignable to type 'Int16Array<ArrayBuffer>'.
    arr_Int16Array = arr_Float64Array;
    //~^ ERROR: Type 'Float64Array<ArrayBuffer>' is not assignable to type 'Int16Array<ArrayBuffer>'.
    arr_Int16Array = arr_Uint8ClampedArray;
    //~^ ERROR: Type 'Uint8ClampedArray<ArrayBuffer>' is not assignable to type 'Int16Array<ArrayBuffer>'.

    arr_Uint16Array = arr_Int8Array;
    //~^ ERROR: Type 'Int8Array<ArrayBuffer>' is not assignable to type 'Uint16Array<ArrayBuffer>'.
    arr_Uint16Array = arr_Uint8Array;
    //~^ ERROR: Type 'Uint8Array<ArrayBuffer>' is not assignable to type 'Uint16Array<ArrayBuffer>'.
    arr_Uint16Array = arr_Int16Array;
    //~^ ERROR: Type 'Int16Array<ArrayBuffer>' is not assignable to type 'Uint16Array<ArrayBuffer>'.
    arr_Uint16Array = arr_Uint16Array;
    arr_Uint16Array = arr_Int32Array;
    //~^ ERROR: Type 'Int32Array<ArrayBuffer>' is not assignable to type 'Uint16Array<ArrayBuffer>'.
    arr_Uint16Array = arr_Uint32Array;
    //~^ ERROR: Type 'Uint32Array<ArrayBuffer>' is not assignable to type 'Uint16Array<ArrayBuffer>'.
    arr_Uint16Array = arr_Float32Array;
    //~^ ERROR: Type 'Float32Array<ArrayBuffer>' is not assignable to type 'Uint16Array<ArrayBuffer>'.
    arr_Uint16Array = arr_Float64Array;
    //~^ ERROR: Type 'Float64Array<ArrayBuffer>' is not assignable to type 'Uint16Array<ArrayBuffer>'.
    arr_Uint16Array = arr_Uint8ClampedArray;
    //~^ ERROR: Type 'Uint8ClampedArray<ArrayBuffer>' is not assignable to type 'Uint16Array<ArrayBuffer>'.

    arr_Int32Array = arr_Int8Array;
    //~^ ERROR: Type 'Int8Array<ArrayBuffer>' is not assignable to type 'Int32Array<ArrayBuffer>'.
    arr_Int32Array = arr_Uint8Array;
    //~^ ERROR: Type 'Uint8Array<ArrayBuffer>' is not assignable to type 'Int32Array<ArrayBuffer>'.
    arr_Int32Array = arr_Int16Array;
    //~^ ERROR: Type 'Int16Array<ArrayBuffer>' is not assignable to type 'Int32Array<ArrayBuffer>'.
    arr_Int32Array = arr_Uint16Array;
    //~^ ERROR: Type 'Uint16Array<ArrayBuffer>' is not assignable to type 'Int32Array<ArrayBuffer>'.
    arr_Int32Array = arr_Int32Array;
    arr_Int32Array = arr_Uint32Array;
    //~^ ERROR: Type 'Uint32Array<ArrayBuffer>' is not assignable to type 'Int32Array<ArrayBuffer>'.
    arr_Int32Array = arr_Float32Array;
    //~^ ERROR: Type 'Float32Array<ArrayBuffer>' is not assignable to type 'Int32Array<ArrayBuffer>'.
    arr_Int32Array = arr_Float64Array;
    //~^ ERROR: Type 'Float64Array<ArrayBuffer>' is not assignable to type 'Int32Array<ArrayBuffer>'.
    arr_Int32Array = arr_Uint8ClampedArray;
    //~^ ERROR: Type 'Uint8ClampedArray<ArrayBuffer>' is not assignable to type 'Int32Array<ArrayBuffer>'.

    arr_Float32Array = arr_Int8Array;
    //~^ ERROR: Type 'Int8Array<ArrayBuffer>' is not assignable to type 'Float32Array<ArrayBuffer>'.
    arr_Float32Array = arr_Uint8Array;
    //~^ ERROR: Type 'Uint8Array<ArrayBuffer>' is not assignable to type 'Float32Array<ArrayBuffer>'.
    arr_Float32Array = arr_Int16Array;
    //~^ ERROR: Type 'Int16Array<ArrayBuffer>' is not assignable to type 'Float32Array<ArrayBuffer>'.
    arr_Float32Array = arr_Uint16Array;
    //~^ ERROR: Type 'Uint16Array<ArrayBuffer>' is not assignable to type 'Float32Array<ArrayBuffer>'.
    arr_Float32Array = arr_Int32Array;
    //~^ ERROR: Type 'Int32Array<ArrayBuffer>' is not assignable to type 'Float32Array<ArrayBuffer>'.
    arr_Float32Array = arr_Uint32Array;
    //~^ ERROR: Type 'Uint32Array<ArrayBuffer>' is not assignable to type 'Float32Array<ArrayBuffer>'.
    arr_Float32Array = arr_Float32Array;
    arr_Float32Array = arr_Float64Array;
    //~^ ERROR: Type 'Float64Array<ArrayBuffer>' is not assignable to type 'Float32Array<ArrayBuffer>'.
    arr_Float32Array = arr_Uint8ClampedArray;
    //~^ ERROR: Type 'Uint8ClampedArray<ArrayBuffer>' is not assignable to type 'Float32Array<ArrayBuffer>'.

    arr_Float64Array = arr_Int8Array;
    //~^ ERROR: Type 'Int8Array<ArrayBuffer>' is not assignable to type 'Float64Array<ArrayBuffer>'.
    arr_Float64Array = arr_Uint8Array;
    //~^ ERROR: Type 'Uint8Array<ArrayBuffer>' is not assignable to type 'Float64Array<ArrayBuffer>'.
    arr_Float64Array = arr_Int16Array;
    //~^ ERROR: Type 'Int16Array<ArrayBuffer>' is not assignable to type 'Float64Array<ArrayBuffer>'.
    arr_Float64Array = arr_Uint16Array;
    //~^ ERROR: Type 'Uint16Array<ArrayBuffer>' is not assignable to type 'Float64Array<ArrayBuffer>'.
    arr_Float64Array = arr_Int32Array;
    //~^ ERROR: Type 'Int32Array<ArrayBuffer>' is not assignable to type 'Float64Array<ArrayBuffer>'.
    arr_Float64Array = arr_Uint32Array;
    //~^ ERROR: Type 'Uint32Array<ArrayBuffer>' is not assignable to type 'Float64Array<ArrayBuffer>'.
    arr_Float64Array = arr_Float32Array;
    //~^ ERROR: Type 'Float32Array<ArrayBuffer>' is not assignable to type 'Float64Array<ArrayBuffer>'.
    arr_Float64Array = arr_Float64Array;
    arr_Float64Array = arr_Uint8ClampedArray;
    //~^ ERROR: Type 'Uint8ClampedArray<ArrayBuffer>' is not assignable to type 'Float64Array<ArrayBuffer>'.

    arr_Uint8ClampedArray = arr_Int8Array;
    //~^ ERROR: Type 'Int8Array<ArrayBuffer>' is not assignable to type 'Uint8ClampedArray<ArrayBuffer>'.
    arr_Uint8ClampedArray = arr_Uint8Array;
    //~^ ERROR: Type 'Uint8Array<ArrayBuffer>' is not assignable to type 'Uint8ClampedArray<ArrayBuffer>'.
    arr_Uint8ClampedArray = arr_Int16Array;
    //~^ ERROR: Type 'Int16Array<ArrayBuffer>' is not assignable to type 'Uint8ClampedArray<ArrayBuffer>'.
    arr_Uint8ClampedArray = arr_Uint16Array;
    //~^ ERROR: Type 'Uint16Array<ArrayBuffer>' is not assignable to type 'Uint8ClampedArray<ArrayBuffer>'.
    arr_Uint8ClampedArray = arr_Int32Array;
    //~^ ERROR: Type 'Int32Array<ArrayBuffer>' is not assignable to type 'Uint8ClampedArray<ArrayBuffer>'.
    arr_Uint8ClampedArray = arr_Uint32Array;
    //~^ ERROR: Type 'Uint32Array<ArrayBuffer>' is not assignable to type 'Uint8ClampedArray<ArrayBuffer>'.
    arr_Uint8ClampedArray = arr_Float32Array;
    //~^ ERROR: Type 'Float32Array<ArrayBuffer>' is not assignable to type 'Uint8ClampedArray<ArrayBuffer>'.
    arr_Uint8ClampedArray = arr_Float64Array;
    //~^ ERROR: Type 'Float64Array<ArrayBuffer>' is not assignable to type 'Uint8ClampedArray<ArrayBuffer>'.
    arr_Uint8ClampedArray = arr_Uint8ClampedArray;

}
