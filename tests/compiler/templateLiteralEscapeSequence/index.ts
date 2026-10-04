// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/templateLiteralEscapeSequence.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare function tag(template: TemplateStringsArray, ...substitutions: any[]): string;

`\u`;                   //~ERROR: Hexadecimal digit expected.
`\u0`;                  //~ERROR: Hexadecimal digit expected.
`\u00`;                 //~ERROR: Hexadecimal digit expected.
`\u000`;                //~ERROR: Hexadecimal digit expected.
`\u0000`;
`\u{}`;                 //~ERROR: Hexadecimal digit expected.
`\u{ffffff}`;           //~ERROR: An extended Unicode escape value must be between 0x0 and 0x10FFFF inclusive.
`\x`;                   //~ERROR: Hexadecimal digit expected.
`\x0`;                  //~ERROR: Hexadecimal digit expected.
`\x00`;
`${0}\u`;               //~ERROR: Hexadecimal digit expected.
`${0}\u0`;              //~ERROR: Hexadecimal digit expected.
`${0}\u00`;             //~ERROR: Hexadecimal digit expected.
`${0}\u000`;            //~ERROR: Hexadecimal digit expected.
`${0}\u0000`;
`${0}\u{}`;             //~ERROR: Hexadecimal digit expected.
`${0}\u{ffffff}`;       //~ERROR: An extended Unicode escape value must be between 0x0 and 0x10FFFF inclusive.
`${0}\x`;               //~ERROR: Hexadecimal digit expected.
`${0}\x0`;              //~ERROR: Hexadecimal digit expected.
`${0}\x00`;
`\u${0}`;               //~ERROR: Hexadecimal digit expected.
`\u0${0}`;              //~ERROR: Hexadecimal digit expected.
`\u00${0}`;             //~ERROR: Hexadecimal digit expected.
`\u000${0}`;            //~ERROR: Hexadecimal digit expected.
`\u0000${0}`;
`\u{}${0}`;             //~ERROR: Hexadecimal digit expected.
`\u{ffffff}${0}`;       //~ERROR: An extended Unicode escape value must be between 0x0 and 0x10FFFF inclusive.
`\x${0}`;               //~ERROR: Hexadecimal digit expected.
`\x0${0}`;              //~ERROR: Hexadecimal digit expected.
`\x00${0}`;
`${0}\u${0}`;           //~ERROR: Hexadecimal digit expected.
`${0}\u0${0}`;          //~ERROR: Hexadecimal digit expected.
`${0}\u00${0}`;         //~ERROR: Hexadecimal digit expected.
`${0}\u000${0}`;        //~ERROR: Hexadecimal digit expected.
`${0}\u0000${0}`;
`${0}\u{}${0}`;         //~ERROR: Hexadecimal digit expected.
`${0}\u{ffffff}${0}`;   //~ERROR: An extended Unicode escape value must be between 0x0 and 0x10FFFF inclusive.
`${0}\x${0}`;           //~ERROR: Hexadecimal digit expected.
`${0}\x0${0}`;          //~ERROR: Hexadecimal digit expected.
`${0}\x00${0}`;

tag`\u`;
tag`\u0`;
tag`\u00`;
tag`\u000`;
tag`\u0000`;
tag`\u{}`;
tag`\u{ffffff}`;
tag`\x`;
tag`\x0`;
tag`\x00`;
tag`${0}\u`;
tag`${0}\u0`;
tag`${0}\u00`;
tag`${0}\u000`;
tag`${0}\u0000`;
tag`${0}\u{}`;
tag`${0}\u{ffffff}`;
tag`${0}\x`;
tag`${0}\x0`;
tag`${0}\x00`;
tag`\u${0}`;
tag`\u0${0}`;
tag`\u00${0}`;
tag`\u000${0}`;
tag`\u0000${0}`;
tag`\u{}${0}`;
tag`\u{ffffff}${0}`;
tag`\x${0}`;
tag`\x0${0}`;
tag`\x00${0}`;
tag`${0}\u${0}`;
tag`${0}\u0${0}`;
tag`${0}\u00${0}`;
tag`${0}\u000${0}`;
tag`${0}\u0000${0}`;
tag`${0}\u{}${0}`;
tag`${0}\u{ffffff}${0}`;
tag`${0}\x${0}`;
tag`${0}\x0${0}`;
tag`${0}\x00${0}`;

tag`0${00}`;              //~ERROR: Octal literals are not allowed.
tag`0${05}`;              //~ERROR: Octal literals are not allowed.
tag`0${000}`;             //~ERROR: Octal literals are not allowed.
tag`0${005}`;             //~ERROR: Octal literals are not allowed.
tag`0${055}`;             //~ERROR: Octal literals are not allowed.
tag`${00}0`;              //~ERROR: Octal literals are not allowed.
tag`${05}0`;              //~ERROR: Octal literals are not allowed.
tag`${000}0`;             //~ERROR: Octal literals are not allowed.
tag`${005}0`;             //~ERROR: Octal literals are not allowed.
tag`${055}0`;             //~ERROR: Octal literals are not allowed.
tag`\0`;
tag`\5`;
tag`\00`;
tag`\05`;
tag`\55`;
tag`\000`;
tag`\005`;
tag`\055`;
tag`${0}\0`;
tag`${0}\5`;
tag`${0}\00`;
tag`${0}\05`;
tag`${0}\55`;
tag`${0}\000`;
tag`${0}\005`;
tag`${0}\055`;
tag`\0${0}`;
tag`\5${0}`;
tag`\00${0}`;
tag`\05${0}`;
tag`\55${0}`;
tag`\000${0}`;
tag`\005${0}`;
tag`\055${0}`;
tag`${0}\0${0}`;
tag`${0}\5${0}`;
tag`${0}\00${0}`;
tag`${0}\05${0}`;
tag`${0}\55${0}`;
tag`${0}\000${0}`;
tag`${0}\005${0}`;
tag`${0}\055${0}`;

tag`\1`;
tag`\01`;
tag`\001`;
tag`\17`;
tag`\017`;
tag`\0017`;
tag`\177`;
tag`\18`;
tag`\018`;
tag`\0018`;
tag`\4`;
tag`\47`;
tag`\047`;
tag`\0047`;
tag`\477`;
tag`\48`;
tag`\048`;
tag`\0048`;
tag`\8`;
tag`\87`;
tag`\087`;
tag`\0087`;
tag`\877`;
tag`\88`;
tag`\088`;
tag`\0088`;
