---
id: Vue
title: Vue.js a Javascript front-end Web Framework
sidebar_label: Vue
---

## Install and Config

### [Install](https://vuejs.org/v2/guide/installation.html)

- for production
  - `<script src="https://cdn.jsdelivr.net/npm/vue"></script>`
- by npm:
  - `npm install -g vue`
  - `npm uninstall -g create-react-ap`
  - `npm update -g vue`
- Vue CLI
  - `npm install -g @vue/cli`
  - `vue --version`
  - `npm update -g @vue/cli`
- Vue Plugins
  - `npm add vue-router`
  - `vue ui`
    - Open a Web GUI to manage Plugin and tasks

### Create and Config Vue App

- `vue create MYAPP`
  - CLI to create project
- `vue ui`
  - go to GUI to create project
- `npm run serve`
- `npm run build`

### DevOps Tools

- Vue Devtools for Chrome extension
- VS code vetur

### SPA Vue hello-world HTML

- Sample Code: helloVue.html

```HTML
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <meta http-equiv="X-UA-Compatible" content="ie=edge" />
    <title>Hello Vue!</title>
    <style>
      body {
        background-color: powderblue;
      }
      h1 {
        color: blue;
        text-align: center;
      }
    </style>
  </head>
  <body>
    <div id="hello-world-app">
      <h1>{{ msg }}</h1>
    </div>
    <script src="https://cdn.jsdelivr.net/npm/vue"></script>
    <script>
      new Vue({
        el: "#hello-world-app",
        data: {
          msg: "Hello Vue!"
        }
      });
    </script>
  </body>
</html>
```

---

## Architecture

<!-- ### Building Blocks -->

### [Vue Instance](https://vuejs.org/v2/guide/instance.html)

- Lifecycle Diagram  
   ![Alt](https://vuejs.org/images/lifecycle.png "Vue Instance Lifecycle Diagram")
  - [LifeCle Hooks](https://vuejs.org/v2/api/#Options-Lifecycle-Hooks)
- [APIs](https://vuejs.org/v2/api/)
- [Data](https://vuejs.org/v2/api/#Options-Data)
- [DOM](https://vuejs.org/v2/api/#Options-DOM)
- [Assets](https://vuejs.org/v2/api/#Options-Assets)
- [Composition](https://vuejs.org/v2/api/#Options-Assets)
- [Misc](https://vuejs.org/v2/api/#Options-Misc)
- A Vue Instance Template Code

```javascript
var vm = new Vue({
  el: '#app',
  data: {
    dataObj1: { }
    dataArr1: [ ]
    data: 'value'
  }
  methods:{
      fun1: function(){  }
      fun2: function(){  }
  }
  filters:{
      capitalize: function() { }
  }
  computed:{
      fun3: function() { }
  }
})
```

### Components

- [Basics](https://vuejs.org/v2/guide/components.html)

### Directives

- v-if expression
- v-if v-else-if v-else condition expression
- condition and ternary expression
  - `"testing > 2"`
  - `"condition ? true : false"`
- v-for expression
  - `v-for="item in items"`
- v-show
- v-on
  - `v-on:click="clickFunc"`
  - `v-on:keyup.enter="enterFunc"`
- v-bind
  - `v-bind:ATTRIBUTE="VALUES"`
  - `:ATTRIBUTE="VALUES"`
- v-text
- v-html
- v-once
- [custom directive guide](https://vuejs.org/v2/guide/custom-directive.html)

### Methords

### Data Binding

- v-mod directive

### Events

### Filters

### Properties

### Components Extend

<!-- ### Structure -->

## MVVM Pattern

### Project Structure

- Folder Structure

        my-app/
        README.md
        node_modules/
        package_lock.json
        package.json
        public/
            index.html
            favicon.ico
        src/
            assets/
                <!-- Static files -->
            components/
                <!-- Components .vue -->
            views/
                Home.vue
                About.vue
                <!-- Views .vue -->
            App.vue
            main.js
            router.js
            store.js
        test/
            unit/
                <!-- TESTING CASES.js -->
        dist/
            css/
                <!--built .css Files -->
            img/
                <!-- built static img Files -->
            js/
                <!-- built .js Files -->
            index.html

### Component .vue file Structure

- `<template> </template>`
  - Components wrapped with cutomized HTML element style
    - `<COMPONENT />`
  - Template Interpolation
    - text replace
      - `{{ var }}`
    - HTML attribute
      - `v-bind`
        - `<div v-bind:class="{alert: showAlert}"> show alert </div>`
        - `<div v-bind:style="{backgroud-color: #00ff00; text-align: center}"> show alert </div>`
      - `v-model`
        - `<input type="text" placeholder="type something..." v-model="DATA_VAR"> {{ DATA_VAR }}</input>`
      - `v-on`
        - `<button v-on:click="ONCLICK_ENVENT_HANDLER">Greet</button>`
        - event modifiers
          - `<button v-on:click.once="ONCLICK_ENVENT_HANDLER">Greet</button>`
            > .stop  
            > .prevent  
            > .capture  
            > .self  
            > .once  
            > .passive
      - `v-for`
      - `v-if`
      - `v-else`
    - Expression Utilities
      - `{{ switch ? switchOn_express : switchOff_express }}`
- `<script> </script>`

  - `import COMPONENTS from 'PATH'`
  - `export default { }`

    ```javascript
      name: ''
      components: { }
      data() { }
      methods: { }
      created() { }
    ```

- `<style> </style>`
  - `scoped` namespacing control
  - `src="PATH_OF_EXTERNAL_CSS_FILE"`

---

## Utilities

### DevOps

#### vue-cli

#### vue-router

#### Testing

### External Packages

#### [vee-validate](https://baianat.github.io/vee-validate/)

- `npm install vee-validate`
- `Vue.use(VeeValidate)`
- `<input v-model="email" v-validate:email="'required|email'" name="field" type="text">`

#### [bootstrap-vue](https://bootstrap-vue.js.org/)

- `npm install bootstrap-vue bootstrap`
- `import BootstrapVue from 'bootstrap-vue'`
- `Vue.use(BootstrapVue)`
- `import 'bootstrap/dist/css/bootstrap.css'`
- `import 'bootstrap-vue/dist/bootstrap-vue.css'`

#### [Lodash](https://lodash.com/docs/4.17.11)

- `npm i --save lodash`
- `var _ = require('lodash');`

---

## Misc

### Learning Resources

- [Vue.js Website](https://vuejs.org/)
- [Vue JS Crash Course - 2019](https://www.youtube.com/watch?v=Wy9q22isx3U)
- [Vuex Crash Course | State Management](https://www.youtube.com/watch?v=5lVQgZzLMHc)
- [Learn Vue.js - Full Course for Beginners - 2019](https://www.youtube.com/watch?v=4deVCNJq3qc)

---
