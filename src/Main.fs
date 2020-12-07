module Main

open Fable.Core.JsInterop

importAll "../styles/main.scss"

open Elmish
open Elmish.React
open Elmish.Debug
open Elmish.HMR

// App
Program.mkSimple App.undoInit App.update App.render
#if DEBUG
|> Program.withDebugger
//|> Program.withConsoleTrace
#endif
|> Program.withReactSynchronous "feliz-app"
|> Program.run