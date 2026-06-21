// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/spreadIdenticalTypesRemoved.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

interface Animal {
    name: string;
    kind: string;
    age: number;
    location: string;
    owner: object;
}

function clonePet(pet: Animal, fullCopy?: boolean) {
    return {
        name: pet.name,
        kind: pet.kind,
        ...(fullCopy && pet),
    }
}

interface Animal2 {
    name: string;
    owner?: string;
}
function billOwner(pet: Animal2) {
    return {
        ...(pet.owner && pet),
        paid: false
    }
}
