---
id: Nodejs
title: Nodejs an asynchronous event driven JavaScript runtime for server side, designed to build scalable network applications
sidebar_label: Nodejs
---

## Overview

---

## Application

### Features

- single-threaded
- non-blocking asynchronous processing
- event-driven
- Real-time web application with Socket.io
- fast
- concurrent

### Applicable Scenarios

- suited for applications where each incoming request requires very few CPU cycles.
- suited for real-time web applications, such as chat rooms, collaboration tools, online games.
  -suited for "Long polling" scenarios.
- RESTful API and Microservices
- CRUD Apps
- IO intensive.
- not for CPU intensive

---

## Architecture

<!-- ### Structures -->

### Reference Model

- System View
  > ![Alt](/img/Nodejs-Architecture-01.png "Nodejs Architecture")
- Event View
  > ![Alt](/img/Nodejs-Architecture-02.png "Nodejs Architecture")

### Project Structrue

- `config`
- `controllers`
- `migrations`
- `models`
- `node_modules`
- `public`
  - `images`
  - `javascripts`
  - `stylesheets`
- `routes`
- `views`
- `package.json`
- `app.js`

### [Global Objects](https://nodejs.org/dist/latest-v12.x/docs/api/globals.html)

- console
- \_\_dirname
- \_\_filename
- require()

<!-- ### Modules -->

### Module and Namespacing

- Module warpping function:
  - `(function( exports, require, module, __filename, __dirname) {})`
- Module exports
  - `module.exports = MODUEL_NAME`
- Module require
  - module in project other file/folder
    - `MODULE = require('./PATH_TO_MODULE')`
  - module in `node-modules` dir/nested
    - `MODULE = require('MODULE_NAME')`

### [Module Lists](https://nodejs.org/docs/latest/api/documentation.html)

- events
- readline
- os
- fs
- path
- zlib
- http
- zlib
- url
- logger
- util

### npm: [Node Package Manager](https://docs.npmjs.com/)

- `npm init`
- `npm install -g PACKAGE`
- `npm install -g PACKAGE@VERSION`
- `npm install -D PACKAGE`
- `npm install --save-dev PACKAGE`
- `npm install -S PACKAGE`
- `npm install --save PACKAGE`
- `npm install --production PACKAGE`
- `npm cache clean --force`
- `npm ls`
- `npm view PACKAGE`
- `npm search PACKAGE`
- `npm update`
- `npm uninstall PACKAGE`
- `npm adduser`
- `npm publish`
- `npm audit fix --force`

- package.json
  - semantic version
    - `PackageName`: Major.Minor.Patch
    - `">PackageName"`: higher than Major.Minor.Patch
    - `"~PackageName"`: Major.Minor.x
    - `"^PackageName"`: Major.x.x
  - npm scripts
    - `npm prestart`
    - `npm start`
    - `npm poststart`
    - `npm stop`
    - `npm restart`
    - `npm test`
    - `npm run CUSTOME_SCRIPTS`
  - npm run all
    - `npm install npm-run-all`
    - `"all": "npm-run-all --parallel SCRIPT_LIST"`
    - `"all": "npm-run-all --serial SCRIPT_LIST"`

### Package Lists

- [express](https://expressjs.com/)
- [socket.io](https://socket.io/)
- [winston](https://github.com/winstonjs/winston)
- [nconf](https://www.npmjs.com/package/nconf)
- [pm2](https://pm2.io/doc/en/runtime/guide/installation/)
- [nodemon](https://github.com/remy/nodemon#nodemon)

---

## Best Practice

### Linux install and upgrade

- [install](https://github.com/nodesource/distributions/blob/master/README.md)

  ```bash
  sudo apt install nodejs npm
  node -v
  npm -v
  sudo npm cache clean -f
  ```

- upgrade

  ```bash
  sudo npm install -g n
  sudo n latest | x.x.x
  sudo n
  node -v
  npm -v
  ```

### Linux/Windows Initialize

- DevOps Env

  ```bash
  cd PROJECT_ROOT
  npm init
  npm install PACKAGES --save
  node SCRIPT.js
  ```

- HelloWorld Node Env

  ```javascript
  var http = require("http");
  http
    .createServer(function(req, res) {
      res.writeHead(200, { "Content-Type": "text/plain" });
      res.end("Hello World\n");
    })
    .listen(8080, "localhost");
  console.log("Server running at http://localhost:8080");
  ```

- HelloWorld Express Env

  ```javascript
  const express = require("express"),
  const app = express();
  app.get("/", function(req, res) {
  res.send("Hello World");
  });
  app.get("*", function(req, res) {
  res.send("Page Not Found", 404);
  });
  app.listen(8080);
  console.log("Express server started on port 8080");
  ```

- HelloWorld MongoDB Env

  ```javascript
  const MongoClient = require("mongodb").MongoClient;
  const assert = require("assert");

  (async function() {
    // Connection URL
    const url = "mongodb://localhost:27017/learn";
    // Database Name
    const dbName = "learn";
    const client = new MongoClient(url, { useNewUrlParser: true });

    try {
      // Use connect method to connect to the Server
      await client.connect();
      console.log("******Connected successfully to server");

      const db = client.db(dbName);
    } catch (err) {
      console.log(err.stack);
    }

    client.close();
    console.log("******Closed successfully to server");
  })();
  ```

### Dockerizing Nodejs Apps

- Dockerfile

```Dockerfile
FROM node:12-slim

# Create app directory
WORKDIR /app/MYAPP/src

# Install app dependencies
# A wildcard is used to ensure both package.json AND package-lock.json are copied
# where available (npm@5+)
COPY package*.json ./

RUN npm install
# If you are building your code for production
# RUN npm ci --only=production

# Bundle app source
COPY . .

EXPOSE 8080
CMD [ "npm", "start" ]
```

- docker-compose.yml

```yaml
version: "3"

services:
  MYAPP:
    build: .
    container_name: MYAPP
    ports:
      - 8080:8080
      - 35729:35729
    volumes:
      - .:/app/MYAPP/src
    working_dir: /app/MYAPP/src
    restart: always
```

- makefile

```makefile
.PHONY : help

APP = MYAPP

help :
  @echo "help commands:"
  @echo "  make start: docker-compose up"
  @echo "  make stop: docker-compose stop"
  @echo "  make restart: docker-compose restart"
  @echo "  make down: docker-compose down"
  @echo "  make remove: docker-compose down"


start :
  @echo "create and run $(APP)"
  sudo docker-compose up -d
  @echo "done......"
  @echo

stop :
  @echo "stop $(APP)"
  sudo docker-compose stop
  @echo "done......"
  @echo

restart :
  @echo "restart $(APP)"
  sudo docker-compose restart
  @echo "done......"
  @echo

down :
  @echo "stop and remove $(APP)"
  sudo docker-compose down
  @echo "done......"
  @echo

remove :
  @echo "remvoe $(APP)"
  sudo docker-compose down
  sudo docker image rm $(APP)
  @echo "done....."
  @echo
```

---

## Misc

### Online Resources

- [Node.js Documentation](https://nodejs.org/en/docs/guides/)
- [W3School Node.js Tutorial](https://www.w3schools.com/nodejs/)
- [Node.js Crash Course](https://www.youtube.com/watch?v=fBNz5xF-Kx4)
- [Express JS Crash Course](https://www.youtube.com/watch?v=L72fhGm1tfE)
- [Node JS Tutorial for Beginners](https://www.youtube.com/watch?v=w-7RQ46RgxU&list=PL4cUxeGkcC9gcy9lrvMJ75z9maRw4byYp)

### Express

#### Routing

#### Templating

#### Middleware
