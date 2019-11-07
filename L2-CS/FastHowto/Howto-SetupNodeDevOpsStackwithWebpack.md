---
id: Howto-SetupNodeDevOpsStackwithWebpack
title: Howto Setup Nodejs DevOps Stack with Webpack
sidebar_label: Nodejs DevOps Stack with Webpack
---

## Backround

- [Node.js](https://nodejs.org/en/)
- [npm](https://docs.npmjs.com/downloading-and-installing-node-js-and-npm)
- [Webpack](https://webpack.js.org/)
  - [Webpack config file](https://webpack.js.org/configuration/)

---

## Prerequisite and Scenario

- Node.js installed and configured for up and running.
- npm installed and configured.

---

## Procedure

### Step 1 - Init Project

- `cd PROJECT_ROOT_DIR`
- `npm init`
- `npm install DEP_PACKAGE`
  - `npm install -S DEP_PACKAGE`

---

### Step 2 - Init Webpack and Plugins

- `npm install -D webpack`
  - `npm install --save-dev webpack`
- `npm install -D webpack-cli`
- `npm install -D webpack-dev-server`
- `npm install -D html-webpack-plugin`
- create `webpack.config.js` in PROJECT_ROOT_DIR

---

### Step 3 - Config Webpack

- `webpack.config.js`

  ```javascript
  const path = require("path");
  const HTMLWebpackPlugin = require("html-webpack-plugin");

  module.exports = {
    entry: "./src/js/index.js", // string | object | array
    output: {
      path: path.resolve(__dirname, "dist"), // string
      filename: "js/bundle.js" // string
    },

    devServer: {
      contentBase: path.join(__dirname, "dist"),
      compress: true, // enable gzip compression
      port: 9999
      // ...
    },
    plugins: [
      new HTMLWebpackPlugin({
        filename: "index.html",
        template: path.resolve(__dirname, "src/index.html")
      })
    ]
  };
  ```

- `package.json`

  ```json
  {
    ...

    "scripts": {
      "dev": "webpack --mode development",
      "build": "webpack --mode production",
      "start:dev": "webpack-dev-server --mode development --open",
      ...
    },

    ...
  }
  ```

---

---

## Result

- Node DevOps Stack setups with webpack, webpack-dev-server, html-webpack-plugin.

---

---

## Pitfalls and Cautions

- config `webpack.config.js` output path the same as html entry path, if not, auto update bundle will not work.

---

---

## Reference Resources

- [No Ref](www.google.com)
