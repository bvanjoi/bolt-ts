// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/typeOfYieldWithUnionInContextualReturnType.ts`, Apache-2.0 License

//@compiler-options: target=esnext
// https://github.com/microsoft/TypeScript/issues/42439

type SyncSequenceFactory = () => Generator<string, string, string>;

type AsyncSequenceFactory = () => AsyncGenerator<string, string, string>;

type SequenceFactory = SyncSequenceFactory | AsyncSequenceFactory

const syncFactory: SyncSequenceFactory = function* (){
  let name = "";
  while(!name){
    name = yield "What is your name?"
  }
  return `That's the end of the game, ${name}`
} 

const asyncFactory: AsyncSequenceFactory = async function* (){
  let name = "";
  while(!name){
    name = yield "What is your name?"
  }
  return `That's the end of the game, ${name}`
} 

const looserSyncFactory: SequenceFactory = function* (){
  let name = "";
  while(!name){
    name = yield "What is your name?"
  }
  return `That's the end of the game, ${name}`
} 

const looserAsyncFactory: SequenceFactory = async function* (){
  let name = "";
  while(!name){
    name = yield "What is your name?"
  }
  return `That's the end of the game, ${name}`
} 
