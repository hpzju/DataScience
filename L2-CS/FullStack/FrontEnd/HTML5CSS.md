---
id: HTML5CSS
title: HTML5 and CSS Shape the WWW
sidebar_label: HTML5 and CSS
---

## Overview

No Introduction

---

## HTML Architecture

### [HTML Elements](https://www.w3schools.com/tags/ref_byfunc.asp)

#### Basic Elements

- `<html>, <meta>, <base>, <title>,<head>, <body>`
- `<!-- .html comments -->`
- refs:
  - [base](https://www.w3schools.com/tags/tag_base.asp)

#### Inline Elements

- `<span>, <b>, <i>, <em>, <strong>, <small>, <mark>, <del>, <ins>, <sub>, <sup>`
- `<a>`
- `&copy;,`

#### Block Elements

- `<h1>, <h2>, <h3>, <h4>, <h5>, <h6>,`
- `<p>, <br>, <hr>`
- `<pre>, <code>`
- `<button>`
  - **display: inline-block**
- `<ol>, <ul>, <li>, <dl>, <dt>, <dd>`
- `<table>, <tr>, <th>, <td>`
- `<form>, <input>, <label>, <select>, <option>, <textarea>, <button>`
- `<heaer>, <nav>, <section>, <article>, <footer>`
- `<iframe>`

#### Multimedia Elements

- `<img>`
  - `<img src="IMAGE_PATH_OR_URL" alt="SHOW_IMG_INFO" width="W" height="H"/>`
- `<svg>`
- `<audio>`
- `<video>`

#### Graphic Elements

- `canvas`
- `map`

---

### [HTML Styling](https://www.w3schools.com/css/default.asp)

#### Inline Style

- `<button sytle="color:#00ff00; margin"> </elem>`

#### Internal Style

- ```html
  <header>
    <!-- .... end other header content -->
    <style>
      body {
        background-color: linen;
      }

      h1 {
        color: maroon;
        margin-left: 40px;
      }
    </style>
  </header>
  ```

#### External Style

- ```html
  <header>
    <!-- .... end other header content -->
    <link rel="stylesheet" type="text/css" href="mystyle.css" />
    <!-- .... start other header content -->
  </header>
  ```

---

## HTML Layout

### HTML Visual Layout

- [Box Model](#box-modelhttpswwww3schoolscomcsscssboxmodelasp)
- [Flexbox](#flexbox)
- [Grid System](#grid)

### HTML Semantic Layout

```html
<header>
  <!-- Header Layout -->
  <nav></nav>
  <!-- Header Layout -->
</header>
<main>
  <section>
    <!-- section Layout -->
    <article>
      <!-- article Layout -->
      <h1>headline</h1>
      <div>
        <aside>aside left</aside>
        <div>
          <p>P1</p>
          <p>P2</p>
          <p>P3</p>
        </div>
        <aside>aside right</aside>
      </div>
      <!-- article Layout -->
    </article>
    <!-- section Layout -->
  </section>
  <!-- more section Layout -->
  <!-- more section Layout -->
  <!-- more section Layout -->
  <!-- more section Layout -->
</main>
<footer></footer>
```

---

## CSS Deepdive

### CSS Reset

- clear all css settings
- provide a baseline to normalize css file
- [refs](https://meyerweb.com/eric/tools/css/reset/)

### SCSS

- `\$parameter = VALUE`
- `background: \$parameter;`
- `@import 'PATH_SCSS'`
- `@extend SELECTOR`
- `@mixin NAME { }`
- `@include NAME();`
- space selector:
  - `h1 { button { $btnColor; } }`
- `&:hover`
- &::after

### Selectors and Precedence

#### Selectors

- Class Selector: `.className`
- ID Selector: `#id`
- Element Selector: `elem`
- Comma Selector, all elem1 and all elem2：`elem1, elem2`
- Combinators
  - space : all p inside div
    - `div p`
  - \> : all p whoes parent is a div (p nested one level in div)
    - `div > p`
  - \+ : all p are placed immediately after div
    - `div + p`
  - ~ : all ul are preceded by p
    - `p ~ ul`
- Psudo Class/Element Selectors
  - :hover
    - `#button1:hover`
  - :first-child
  - :last-child
  - :nth-child(n)
  - ::first-letter
  - ::last-letter
  - ::first-line
  - ::selction
  - ::after
  - ::before
- [see more](https://www.w3schools.com/cssref/css_selectors.asp)

#### Rules

- inline css ( html style attribute ) overrides css rules in style tag and css file
- a more specific selector takes precedence over a less specific one
- rules that appear later in the code override earlier rules if both have the same specificity.
- A css rule with **!important** always takes precedence.

#### Selectors and Specificity Hierarchy

- Inline styles
- IDs
- Classes, attributes and pseudo-classes
- Elements and pseudo-elements

---

### Psudo Element

- :root
- :active
- :visited
- :hover
- :link
- ::after
  - content: ''
- ::before
  - content: ''

### [CSS @media Query](https://www.w3schools.com/cssref/css3_pr_mediaquery.asp)

- Syntax
  - `@media not|only mediatype and (mediafeature and|or|not mediafeature) { CSS-Code; }`
- [Bootstrap 4 Example](https://getbootstrap.com/docs/4.3/layout/overview/)

  ```css
    //------------- scss version -------------
    @include media-breakpoint-only(xs) {
      ...;
    }
    @include media-breakpoint-only(sm) {
      ...;
    }
    @include media-breakpoint-only(md) {
      ...;
    }
    @include media-breakpoint-only(lg) {
      ...;
    }
    @include media-breakpoint-only(xl) {
      ...;
    }
    //------------- range version -------------
    // Extra small devices (portrait phones, less than 576px)
    @media (max-width: 575.98px) {
      ...;
    }

    // Small devices (landscape phones, 576px and up)
    @media (min-width: 576px) and (max-width: 767.98px) {
      ...;
    }

    // Medium devices (tablets, 768px and up)
    @media (min-width: 768px) and (max-width: 991.98px) {
      ...;
    }

    // Large devices (desktops, 992px and up)
    @media (min-width: 992px) and (max-width: 1199.98px) {
      ...;
    }

    // Extra large devices (large desktops, 1200px and up)
    @media (min-width: 1200px) {
      ...;
    }`
  ```

### CSS Functions

- calc
  - +, -, \*, /

### Basic CSS Properties Categories

#### Display

- [background](https://www.w3schools.com/cssref/css3_pr_background.asp): bg-color bg-image position/bg-size bg-repeat bg-origin bg-clip bg-attachment initial|inherit
  - background-size
  - background-repeat
  - background-position
  - background-attachment
  - line-gradient
- [display](https://www.w3schools.com/cssref/pr_class_display.asp): inline, block, none, flex
- color: #FFFFFF | rgb(0,0,0,0.5)
- float: left, right inherit, none
- overflow: auto, hidden, inherit, scroll, visible
- clear: both, left, right, inherit, none

#### Color

- color:
  - #FFFFFF
  - rgb(255,255,255)
  - rgba(255,255,255,1.0)
  - blue, green, red...
- background-color

#### Mesurement

- px
- em
  - base value inherited from root value
  - default 16px
- rem
- vw
- vh
- %

#### Sizing

- width:
  - vw
- Height:
  - vh

#### [Box Model](https://www.w3schools.com/css/css_boxmodel.asp)

- margin
  - margin:
  - margin-top, margin-right, margin-bottom, margin-left
  - margin-x
  - margin-y
- [border:](https://www.w3schools.com/css/css_border.asp)
  - border:
  - border-top, border-right, border-bottom, border-left
  - border-style
  - border-color
  - border-width
  - border-radius
- padding:
  - padding:
  - padding-top, padding-right, padding-bottom, padding-left
  - padding-x
  - padding-y
- outline
  - outline-offset
  - outline-color
- content:

#### Placement

- text-align: justify, left, center, right
- position: static|absolute|fixed|relative|sticky|initial|inherit;
  - top, bottom, left, right:
- float: left, right inherit, none
- overflow: auto, hidden, inherit, scroll, visible
- clear: both, left, right, inherit, none
- z-index:
  - n, -n
  - position: static will disable z-index

#### Transform

- transform
  - translateX()
  - translateY()
  - translateZ()
  - rotate(), rotateZ()
  - rotate3D()
  - rotateX()
  - rotateY()
  - skew(), skewX()
  - skewY()
  - skewZ()

#### Transition

- transition: property duration timing-function delay|initial|inherit;
- transition-property
  - none|all|property|initial|inherit;
- transition-duration
- transition-timing-function
  - linear|ease|ease-in|ease-out|ease-in-out|step-start|step-end|steps(int,start|end)|cubic-bezier(n,n,n,n)|initial|inherit
- transition-delay

#### Animation

- animation: animationname duration timing-function delay iteration-count direction fill-mode play-state
- animation-name
- animation-duration
- animation-timing-function
- animation-delay
- animation-iteration-count
- animation-direction
- animation-fill-mode
- animation-play-state
- @keyframes
  - @keyframes animationname {keyframes-selector {css-styles;}}

#### Flexbox

- display: flex
- flex-direction: row, column
- flex-basis: 20%
- align-items: flex-start, flex-end, center, stretch
- justify-content
- flex-warp: warp
- @media(min-width: 786px)

#### Grid

- grid

## HTML Component

### [Form](https://www.w3schools.com/html/html_forms.asp)

- form properties
  - action
  - method
  - target
- form elements
  - label
  - input
    - [type](https://www.w3schools.com/html/html_form_input_types.asp)
      - text
      - button
      - radio
      - checkbox
      - submit
    - name
    - value
  - select
    - option

### [Table](https://www.w3schools.com/html/html_tables.asp)

### Text and Typography

- Special Charactors
  - `&lt;` : <
  - `&gt;` : >
  - `&amp` : &
  - `&copy`: ©
- [Google Fonts](https://fonts.google.com/)
  - import
  - link
- Attributes
  - color
  - font-family
  - font-size
  - font-style
  - font-weight
  - line-height
  - letter-spacing
  - word-spacing
  - text-decoration
  - text-transform
    - uppercase
    - lowercase
    - capitalize

---

## Misc

### Tools

- [Emmet](https://emmet.io/)

### Frameworks

- [Bootstrap4](https://getbootstrap.com/docs/4.3/getting-started/introduction/)

### Learning Resources

- [HTML5 Tutorial](https://www.w3schools.com/html/default.asp)
- [Learn HTML5 and CSS3 From Scratch - Full Course](https://www.youtube.com/watch?v=mU6anWqZJcc)
- [Learn HTML5 & CSS3 From Scratch - Make Responsive Websites](https://www.youtube.com/watch?v=hefT68D-zTA)

### Standards

- [ALL STANDARDS AND DRAFTS about HTML](https://www.w3.org/TR/?tag=html)
- [HTML5: W3C Recommendation TR](https://www.w3.org/TR/html/)

---
