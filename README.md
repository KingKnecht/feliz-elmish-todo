## Todo List with Undo/Redo

Todo list with undo/redo functionality including non persistent updates (e.g. when writing to an input box), transactions for bulk edits and jumping directly to a present state.
Todo list is taken from the great book [The Elmish Book](https://zaid-ajaj.github.io/the-elmish-book/#/).

UndoList is more or less my own implementation and far from being Fsharp/functional idiomatic. Working on this...
Front-end is using [Feliz Bulma](https://github.com/Dzoukr/Feliz.Bulma). But to be honest I have very little knowledge of web front-ends. Working on this too...

![Todo](https://user-images.githubusercontent.com/4339172/101689307-4ea1b980-3a6d-11eb-96c0-0edcef03e775.gif)

## Requirements

* [dotnet SDK](https://www.microsoft.com/net/download/core) 2.0.0 or higher
* [node.js](https://nodejs.org) 10.0.0 or higher

## Development

Before doing anything, start with installing npm dependencies using `npm install`.

Then to start development mode with hot module reloading, run:
```bash
npm start
```
This will start the development server after compiling the project, once it is finished, navigate to http://localhost:8080 to view the application .

To build the application and make ready for production:
```
npm run build
```
This command builds the application and puts the generated files into the `deploy` directory (can be overwritten in webpack.config.js).


