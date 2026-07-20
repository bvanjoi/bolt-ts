// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/contextualTypeArrayReturnType.ts`, Apache-2.0 License

interface IBookStyle {
    initialLeftPageTransforms?: (width: number) => NamedTransform[];
}

interface NamedTransform {
    [name: string]: Transform3D;
}

interface Transform3D {
    cachedCss: string;
}

var style: IBookStyle = {
    initialLeftPageTransforms: (width: number) => {
    //~^ ERROR: Type '(width: number) => { ry: null; }[]' is not assignable to type 'undefined | ((width: number) => NamedTransform[])'.
        return [
            {'ry': null }
        ];
    }
}

