## Todo List with Undo/Redo

Todo list with undo/redo functionality including non persistent updates (e.g. when writing to an input box) and transactions for bulk edits.
Todo list is taken from the great book [The Elmish Book](https://zaid-ajaj.github.io/the-elmish-book/#/).

UndoList is more or less my own implementation and far from beeing Fsharp/functional idiomatic. Working on this...
Front-end is using [Feliz Bulma](https://github.com/Dzoukr/Feliz.Bulma). But to be honest I have very little knowledge of web front-ends. Working on this too...


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

### Tests

The template includes a test project that ready to go which you can either run in the browser in watch mode or run in the console using node.js and mocha. To run the tests in watch mode:
```
npm run test:live
```
This command starts a development server for the test application and makes it available at http://localhost:8085.

To run the tests using the command line and of course in your CI server, you have to use the mocha test runner which doesn't use the browser but instead runs the code using node.js:
```
npm test
```
