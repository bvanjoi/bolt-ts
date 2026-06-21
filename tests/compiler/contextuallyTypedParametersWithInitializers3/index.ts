// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contextuallyTypedParametersWithInitializers3.ts`, Apache-2.0 License

//@compiler-options: target=esnext
//@compiler-options: strict
//@compiler-options: noEmit

type CanvasDirection = "RIGHT" | "LEFT";

interface GraphActions {
  setDirection: (direction: CanvasDirection) => void;
}

export declare function create<T>(config: T): void;

declare function takesDirection(direction: CanvasDirection): void;

create<GraphActions>({
  setDirection: (direction = "RIGHT") => {
    takesDirection(direction);
  },
});
