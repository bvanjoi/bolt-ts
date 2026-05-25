// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/comparableRelationBidirectional.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

enum AutomationMode {
    NONE = "",
    TIME = "time",
    SYSTEM = "system",
    LOCATION = "location",
}

interface ThemePreset {
    id: string;
}

interface Automation {
    mode: AutomationMode;
}

interface UserSettings {
    presets: ThemePreset[];
    automation: Automation;
}

interface ExtensionData {
    settings: UserSettings;
}

export function getMockData(): ExtensionData {
    return {
        settings: {
            presets: [],
            automation: {
                mode: "",
            },
        } as UserSettings,
    }
}
