# "Picks and Bans" Application
Simple application for a "picks and bans" system.

## Development
This application is written in Elm. There is a compilation step into JS, and then a bundling step; these are both handled by Webpack with `src/index.js` as the entry point and `src/index.html` as the shell.

In order to run the project locally:

- Go to the project root directory
- `yarn` or `npm install` to install the dependencies
- `yarn run dev` or `npm run dev`
- Browse to `localhost:3000`
- Webpack should handle Hot Module Replacement / Reloading as you develop

Any additions to Elm files under `src/` should be picked up automatically by the loader. You can change the Elm source directory in `elm-package.json`.

The project uses [tachyons](http://tachyons.io) for styling, as it meshes well with the reusable views of Elm. Have a look at their guides, it is quite pleasant :)

### Customising maps
You will notice that the map assets are missing. Here is how to add your own:
- `mkdir images/`
- Add respective images; they will be copied over to `dist/images/` during bundling
- Customise `initMaps : List Map` in `Model.elm` to match your map names, img src

## Deployment
Simply run `webpack -p` to bundle and minify the scripts/html/assets into `dist/`.
Deploy the `dist/` directory with your favourite host/server/static service.

