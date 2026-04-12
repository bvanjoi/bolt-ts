// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/externalModuleReferenceDoubleUnderscore1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare module 'timezonecomplete' {
    import basics = require("__timezonecomplete/basics");
    export import TimeUnit = basics.TimeUnit;
}

declare module '__timezonecomplete/basics' {
    export enum TimeUnit {
        Second = 0,
        Minute = 1,
        Hour = 2,
        Day = 3,
        Week = 4,
        Month = 5,
        Year = 6,
    }
}