// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/narrowingConstrainedTypeParameter.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strictNullChecks

interface Pet {
    name: string;
}

function isPet(pet: any): pet is Pet {
    return typeof pet.name === "string";
}

export function speak<TPet extends Pet>(pet: TPet, voice: (pet: TPet) => string): string {
    if (!isPet(pet)) {
        throw new Error("Expected \"pet\" to be a Pet");
    }
    return voice(pet);
}