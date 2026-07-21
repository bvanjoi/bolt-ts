// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/objectLitGetterSetter.ts`, Apache-2.0 License

//@compiler-options: target=es2015

            var obj = {};
            Object.defineProperty(obj, "accProperty", <PropertyDescriptor>({
                get: function () {
                    eval("public = 1;");
                    return 11;
                },
                set: function (v) {
                }
            }))
