# bolt-ts

bolt-ts is a TypeScript compiler implemented in Rust. The current implementation heavily leverages code ported from the original TypeScript compiler(tsc).

## Performance

When testing a subset of type-fest functionality, bolt-ts demonstrates:

- 2.5× faster compilation than ts-go
- 5× faster than tsc
(Benchmarked on Apple M3 Max with 36GB RAM. See [typescript-compiler-bench](https://github.com/bvanjoi/typescript-compiler-bench) for details)

## Current Status

Core functionalities are operational but require refinement. Key pending improvements include:

- Parser: async function handling, regular expression parsing.
- Module Resolution: cache, `exports`/`imports` field support, `node_modules/@types` type definition resolution.
- Type Checking: enum implementation, Late-bound symbol check, Various edge-case bugs.
- Output Generation: Sourcemap generation, different module systems.
- And others: JavaScript file processing, Language Service..

