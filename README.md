# bolt-ts

bolt-ts is a TypeScript compiler implemented in Rust. The current implementation heavily leverages code ported from the original TypeScript compiler(tsc).

## Performance

When testing a subset of `type-fest` functionality, bolt-ts demonstrates:

- 2.5× faster than ts-go
- 5× faster than tsc
(Benchmarked on Apple M3 Max with 36GB RAM. See [typescript-compiler-bench](https://github.com/bvanjoi/typescript-compiler-bench) for details)

## Current Status

Core functionalities are operational but require refinement. Key pending improvements include:

- Parser: async function, with stmt.
- Module Resolution: cache, `exports`/`imports` field support, `node_modules/@types` type definition resolution.
- Type Checking: various edge-case bugs.
- Output Generation: sourcemap generation, different module systems.
- And others: js file processing, language service..

