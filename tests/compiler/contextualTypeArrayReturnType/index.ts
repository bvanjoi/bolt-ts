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
        return [
            {'ry': null }
        ];
    }
}

