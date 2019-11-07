---
id: Markdown
title: Markdown
sidebar_label: Markdown
---

## URL Link

- Github [GFM](https://github.github.com/gfm/)
- Github [Write Formatting](https://help.github.com/en/articles/basic-writing-and-formatting-syntax)
- Github [Mastering Markdown](https://guides.github.com/features/mastering-markdown/)
- Wordpress [Markdown Quick Reference](https://en.support.wordpress.com/markdown-quick-reference/)
- [Markdown Guide](https://www.markdownguide.org/)

---

## Lable Link

- Some text with [a local link][1]
- and [another local link][2]
- [Section link](#emphasize)

---

## Image Link

- Logo: ![Alt](/img/Markdown-Markdown.png "Markdown Logo")
- this is a image label: ![Alt][3]

---

## Headers

# H1

## H2

### H3

#### H4

##### H5

###### H6

---

## Lists

### Bullet List

- item1
- item2
- item3

### Number List

1. num1
2. num2
3. num3

### Task List

- [x] @mentions, #refs, [links](), **formatting**, and <del>tags</del> supported
- [x] list syntax required (any unordered or ordered list supported)
- [x] this is a complete item
- [ ] this is an incomplete item

### Nested List

- Level1-1
- Level1-2
  1. Level2-1
     - Level3-1
     - Level3-2
  2. Level2-2
- Level1-3
  - Level2-1
    - Level3-1
      - Level4-1
        - Level5-1

---

## Blockquotes

> Level1-Block1
>
> > Level2-Block1
> >
> > > Level3-Block1

> Level1-Block2
>
> - Item1
> - Item2

---

## Pre-formatted

    Begin each line with
    Tab or more spaces to
    make text look
    e x a c t l y
    like you type i
    t.

---

## Code formatted

### Code Line formatted

`import pymongo`

### Code Block formatted

```python
import pymongo

# establish a connection to the database

client = pymongo.MongoClient("mongodb://localhost")

db_school = client.school
col_grades = db_school.grades
```

---

## Emphasize

- _Itali with one star_
- **Bold with two stars**
- ~~This was mistaken text~~
- **This text is _Bold and Italic_ important**

---

## Escape Formatting

Let's rename \*our-new-project\* to \*our-old-project\*.

---

## Tables

| First Header                | Second Header                               |
| --------------------------- | ------------------------------------------- |
| Content from cell 1         | Content from cell 2                         |
| Content in the first column | Content in the second column with long line |

---

---

## LaTex Math Equation

### Inline and Block

This is inline formula: $\sum_{i=1}^{\infty} \frac{1}{n^s} = \prod_p \frac{1}{1 - p^{-s}}$, end of line.

This is Block Formula:
$$\sum_{i=1}^{\infty} \frac{1}{n^s} = \prod_p \frac{1}{1 - p^{-s}}$$
end of block.

### Spacing

- `\quad` space equal to the current font size (= 18 mu)
- `\,` space equal to 3/18 of \quad (= 3 mu)
- `\:`, space equal to 4/18 of \quad (= 4 mu)
- `\;`, space equal to 5/18 of \quad (= 5 mu)
- `\!`, space equal to -3/18 of \quad (= -3 mu)
- `\ (space after backslash!)`, space equal to equivalent of space in normal text
- `\qquad`, space equal to twice of \quad (= 36 mu)

$$A \quad B$$
$$A \, B$$
$$A \; B$$
$$A \! B$$
$$A \ B$$
$$A \qquad B$$
$$A  B$$

### Subscript and Superscript

$$BLOCK_{subscript}^{superscript}$$

$$BLOCK_{subscript_{nestedsubscript}}^{superscript^{nestedsuperscript}}$$

### Special Symbols

#### Brackets and Parentheses

$$\big($$
$$\Big($$
$$\bigg($$
$$\Bigg($$
$$\big]$$
$$\Big]$$
$$\bigg]$$
$$\Bigg]$$
$$\big\{$$
$$\Big\{$$
$$\bigg\{$$
$$\Bigg\{$$
$$ \big \langle $$
$$ \Big \langle $$
$$ \bigg \langle $$
$$ \Bigg \langle $$
$$ \big \rangle $$
$$ \Big \rangle $$
$$ \bigg \rangle $$
$$ \Bigg \rangle $$

#### Integrals

$$\int_{i=1}^n f(x) dx$$
$$\iint_V \mu(u,v) \,du\,dv$$
$$\iiint_V \mu(u,v,w) \,du\,dv\,dw$$

$$\oint_V f(s) \,ds$$
$$\oiint_V f(s,t) \,ds\,dt$$

#### Sum, Product, Limit

$$\sum_{n=1}^{\infty} 2^{-n} = 1$$

$$\prod_{i=a}^{b} f(i)$$

$$\lim_{x\to\infty} f(x)$$

$$\int\limits_{a}^{b} x^2 dx$$

#### Fractions and Binomials

$$f(x)=\frac{P(x)}{Q(x)}$$

$$\frac{1+\frac{a}{b}}{1+\frac{1}{1+\frac{1}{a}}}$$

$$a_0+\cfrac{1}{a_1+\cfrac{1}{a_2+\cfrac{1}{a_3+\cdots}}}$$

$$\binom{n}{k} = \frac{n!}{k!(n-k)!}$$

#### Sets

$$\cup_{i=1}^n A_{i}$$
$$\cap_{i=1}^n A_{i}$$
$${3x^2 \in R \subset Q}$$

### Greek Letters

[List of Greek letters and math symbols](https://www.overleaf.com/learn/latex/List_of_Greek_letters_and_math_symbols)

### Mathematical operators

---

---

[1]: http://google.com/ "Google"
[2]: http://youtube.com/ "Youtube"
[3]: /img/Markdown-logo.png "Markdown-logo.png"
