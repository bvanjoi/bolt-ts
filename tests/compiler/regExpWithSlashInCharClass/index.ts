// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/regExpWithSlashInCharClass.ts`, Apache-2.0 License

var foo1 = "a/".replace(/.[/]/, "");
var foo2 = "a//".replace(/.[//]/g, "");
var foo3 = "a/".replace(/.[/no sleep /till/]/, "bugfix");

var re0 = /* ... *//w+/;
var re1 = // ...
            /w+/;
var re2 =
/*
 ...
 */
 /w+/;

var match = /reg/.exec('');
