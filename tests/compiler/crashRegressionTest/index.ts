// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/crashRegressionTest.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace MsPortal.Util.TemplateEngine {
    "use strict";
 
    interface TemplateKeyValue {
        [name: string]: string;
    }
 
    class StringTemplate {
        private _templateStorage: TemplateStorage;
 
        constructor(templateStorage: TemplateStorage) {
            this._templateStorage = templateStorage;
        }
 
        public text(value?: string): any {
            this._templateStorage.templateSources[this._name] = value;
            //~^ ERROR: Property '_name' does not exist on type 'MsPortal.Util.TemplateEngine.StringTemplate<StringTemplate>'.
            //~| ERROR: Type 'undefined | string' is not assignable to type 'string'.
        }
    }
 
    export class TemplateStorage {
        public templateSources: TemplateKeyValue = {};
        public templateData: TemplateKeyValue = {};
    }
}