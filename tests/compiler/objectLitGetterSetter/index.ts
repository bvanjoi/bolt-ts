// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/objectLitGetterSetter.ts`, Apache-2.0 License

//@compiler-options: target=esnext

            var obj = {};
            Object.defineProperty(obj, "accProperty", <PropertyDescriptor>({
                get: function () {
                    eval("public = 1;");
                    return 11;
                },
                set: function (v) {
                }
            }))
