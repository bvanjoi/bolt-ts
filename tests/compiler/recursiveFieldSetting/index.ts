// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/recursiveFieldSetting.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class Recursive1 {
    constructor(private readonly parent?: Recursive1) {}
    private depth: number = this.parent ? this.parent.depth + 1 : 0;
}

class Recursive2 {
    parent!: Recursive2;
    depth: number = this.parent.depth;
}

class Recursive3 {
    parent!: Recursive3;
    depth: number = this.parent.alpha;
    alpha = 0;
}