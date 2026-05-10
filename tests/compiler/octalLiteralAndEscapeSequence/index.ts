// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/octalLiteralAndEscapeSequence.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

00;               //~ERROR: Octal literals are not allowed.
05;               //~ERROR: Octal literals are not allowed.
000;              //~ERROR: Octal literals are not allowed.
005;              //~ERROR: Octal literals are not allowed.
055;              //~ERROR: Octal literals are not allowed.
`0${00}`;         //~ERROR: Octal literals are not allowed.
`0${05}`;         //~ERROR: Octal literals are not allowed.
`0${000}`;        //~ERROR: Octal literals are not allowed.
`0${005}`;        //~ERROR: Octal literals are not allowed.
`0${055}`;        //~ERROR: Octal literals are not allowed.
`${00}0`;         //~ERROR: Octal literals are not allowed.
`${05}0`;         //~ERROR: Octal literals are not allowed.
`${000}0`;        //~ERROR: Octal literals are not allowed.
`${005}0`;        //~ERROR: Octal literals are not allowed.
`${055}0`;        //~ERROR: Octal literals are not allowed.

"\0";
"\5";             //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x05'.
"\00";            //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x00'.
"\05";            //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x05'.
"\55";            //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x2d'.
"\000";           //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x00'.
"\005";           //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x05'.
"\055";           //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x2d'.
'\0';
'\5';             //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x05'.
'\00';            //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x00'.
'\05';            //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x05'.
'\55';            //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x2d'.
'\000';           //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x00'.
'\005';           //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x05'.
'\055';           //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x2d'.

"\1";             //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x01'.
"\01";            //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x01'.
"\001";           //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x01'.
"\17";            //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x0f'.
"\017";           //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x0f'.
"\0017";          //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x01'.
"\177";           //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x7f'.
"\18";            //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x01'.
"\018";           //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x01'.
"\0018";          //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x01'.
"\4";             //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x04'.
"\47";            //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x27'.
"\047";           //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x27'.
"\0047";          //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x04'.
"\477";           //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x27'.
"\48";            //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x04'.
"\048";           //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x04'.
"\0048";          //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x04'.
"\8";             //~ERROR: Escape sequence '\8' is not allowed.
"\87";            //~ERROR: Escape sequence '\8' is not allowed.
"\087";           //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x00'
"\0087";          //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x00'.
"\877";           //~ERROR: Escape sequence '\8' is not allowed.
"\88";            //~ERROR: Escape sequence '\8' is not allowed.
"\088";           //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x00'.
"\0088";          //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x00'.
'\1';             //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x01'.
'\01';            //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x01'.
'\001';           //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x01'.
'\17';            //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x0f'.
'\017';           //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x0f'.
'\0017';          //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x01'.
'\177';           //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x7f'.
'\18';            //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x01'.
'\018';           //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x01'.
'\0018';          //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x01'.
'\4';             //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x04'.
'\47';            //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x27'.
'\047';           //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x27'.
'\0047';          //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x04'.
'\477';           //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x27'.
'\48';            //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x04'.
'\048';           //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x04'.
'\0048';          //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x04'.
'\8';             //~ERROR: Escape sequence '\8' is not allowed.
'\87';            //~ERROR: Escape sequence '\8' is not allowed.
'\087';           //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x00'
'\0087';          //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x00'. 
'\877';           //~ERROR: Escape sequence '\8' is not allowed.   
'\88';            //~ERROR: Escape sequence '\8' is not allowed.
'\088';           //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x00'.
'\0088';          //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x00'.

`\0`;
`\5`;             //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x05'.
`\00`;            //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x00'.
`\05`;            //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x05'.
`\55`;            //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x2d'.
`\000`;           //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x00'.
`\005`;           //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x05'.
`\055`;           //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x2d'.
`${0}\0`;
`${0}\5`;         //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x05'.
`${0}\00`;        //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x00'.
`${0}\05`;        //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x05'.
`${0}\55`;        //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x2d'.
`${0}\000`;       //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x00'.
`${0}\005`;       //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x05'.
`${0}\055`;       //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x2d'.
`\0${0}`;
`\5${0}`;         //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x05'.
`\00${0}`;        //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x00'.
`\05${0}`;        //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x05'.
`\55${0}`;        //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x2d'.
`\000${0}`;       //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x00'.
`\005${0}`;       //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x05'.
`\055${0}`;       //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x2d'.
`${0}\0${0}`;
`${0}\5${0}`;     //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x05'.
`${0}\00${0}`;    //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x00'.
`${0}\05${0}`;    //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x05'.
`${0}\55${0}`;    //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x2d'.
`${0}\000${0}`;   //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x00'.
`${0}\005${0}`;   //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x05'.
`${0}\055${0}`;   //~ERROR: Octal escape sequences are not allowed. Use the syntax '\x2d'.
