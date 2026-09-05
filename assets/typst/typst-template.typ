// Typst template of the thesis PDF. Quarto calls `article(...)` with the
// document metadata; the body of the thesis is `doc`.
//
// Typography follows the submitted Word thesis: David 12 pt, double spacing,
// justified paragraphs with a half-inch first-line indent, blue numbered
// headings, every chapter on a new page, captions above figures and tables.

#let thesis-fonts = ("David", "David CLM", "Frank Ruehl CLM", "Noto Serif Hebrew", "libertinus serif")
#let heading-blue = rgb("2F5496")

#let article(
  title: none,
  subtitle: none,
  authors: none,
  date: none,
  abstract: none,
  abstract-title: none,
  cols: 1,
  lang: "he",
  region: "IL",
  font: thesis-fonts,
  fontsize: 12pt,
  title-size: 1.5em,
  subtitle-size: 1.25em,
  heading-family: thesis-fonts,
  heading-weight: "regular",
  heading-style: "normal",
  heading-color: heading-blue,
  heading-line-height: 0.65em,
  sectionnumbering: none,
  toc: false,
  toc_title: none,
  toc_depth: none,
  toc_indent: 1.5em,
  doc,
) = {
  // Quarto passes `mainfont` as a one-element tuple; keep the fallbacks.
  let fonts = if type(font) == array { font + thesis-fonts } else { (font,) + thesis-fonts }

  set page(numbering: "1", number-align: center)
  set text(lang: lang, region: region, font: fonts, size: fontsize)
  set par(justify: true, leading: 1.2em, spacing: 1.2em, first-line-indent: (amount: 0.5in, all: true))
  set heading(numbering: sectionnumbering)

  show heading: it => {
    set text(fill: heading-blue, weight: "regular")
    set par(first-line-indent: 0pt)
    it
  }
  // Chapters start on a new page; the break itself is inserted by
  // filters/title-page.lua between top-level blocks.
  show heading.where(level: 1): it => {
    v(0.5em)
    text(size: 16pt)[#it]
    v(0.8em)
  }
  show heading.where(level: 2): it => {
    v(0.4em)
    text(size: 13pt)[#it]
    v(0.5em)
  }
  show heading.where(level: 3): it => {
    v(0.3em)
    text(size: 12pt)[#it]
    v(0.4em)
  }

  // Captions above figures and tables, in the heading blue like the thesis.
  show figure: set figure.caption(position: top)
  show figure.caption: it => {
    set text(fill: rgb("44546A"))
    set par(first-line-indent: 0pt)
    it
  }
  show figure.where(kind: table): set figure.caption(position: top)

  // Lists and quotes without the paragraph indent.
  show list: set par(first-line-indent: 0pt)
  show enum: set par(first-line-indent: 0pt)
  show quote: set par(first-line-indent: 0pt)
  show outline: set par(first-line-indent: 0pt, leading: 0.65em, spacing: 0.9em)
  // A footnote inside a caption belongs to the figure, not to the list.
  show outline: it => {
    show footnote: none
    it
  }
  show outline.entry: it => text(size: 12pt, fill: black)[#it]

  show footnote.entry: set text(size: 10pt)
  show footnote.entry: set par(leading: 0.5em, first-line-indent: 0pt)

  if cols == 1 {
    doc
  } else {
    columns(cols, doc)
  }
}

// Tables are set in 10 pt, as in the submitted thesis.
#show table: set text(size: 10pt)
#show table: set par(first-line-indent: 0pt, leading: 0.6em)
#set table(
  inset: 6pt,
  stroke: none
)
