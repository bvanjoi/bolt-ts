// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/uncaughtCompilerError1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

declare var index, lineTokens, token, tokens;

function f() {
    if (lineTokens[index].trim() === '=' && index > 0 && token.type === '' && tokens[index - 1].type === 'attribute.name.html') {
        if (index === (tokens.length - 1)) {
            return { appendText: '\"\"', advanceCount: 1 };
        }
        else if (tokens[index + 1].type !== 'attribute.value.html' && tokens[index + 1].type !== '') {
            return { appendText: '\"\"', advanceCount: 1 };
        }
        return null;
    }
}
