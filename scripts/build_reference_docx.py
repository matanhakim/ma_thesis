"""Build the Word reference document (assets/reference.docx) for Quarto.

Quarto renders the thesis to Word through Pandoc, which takes page setup,
fonts and paragraph styles from a reference document. The reference document
is derived from the submitted thesis so that the rendered Word file inherits
its typography (David 12 pt, double spacing, first-line indent, heading
colours, US Letter page with 1-inch margins, right-to-left section settings).

The script empties the thesis document of its content, removes the automatic
heading numbering (Quarto numbers the sections itself) and adds the paragraph
styles Pandoc expects, each based on the thesis' own styles.

Usage: python scripts/build_reference_docx.py <path to thesis .docx>
"""
import copy
import sys
from pathlib import Path

from docx import Document
from docx.enum.style import WD_STYLE_TYPE
from docx.oxml.ns import qn

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / "assets" / "reference.docx"

# Pandoc paragraph styles and the thesis style each one should inherit from.
PARAGRAPH_STYLES = {
    "Body Text": "Normal",
    "First Paragraph": "Body Text",
    "Compact": "Body Text",
    "Block Text": "Body Text",
    "Image Caption": "Caption",
    "Table Caption": "Caption",
    "Figure": "Normal",
    "Captioned Figure": "Figure",
    "Footnote Text": "Normal",
    "Bibliography": "Normal",
    "Source Code": "Normal",
    "TOC Heading": "Heading 1",
    "Abstract": "Normal",
    "Abstract Title": "Heading 1",
    "Title": "Title",
    "Subtitle": "Normal",
    "Author": "Normal",
    "Date": "Normal",
}
CHARACTER_STYLES = {
    "Footnote Reference": None,
    "Hyperlink": None,
    "Verbatim Char": None,
}


def clear_body(document):
    """Remove every block of the body but keep the final section properties."""
    body = document.element.body
    for child in list(body):
        if child.tag != qn("w:sectPr"):
            body.remove(child)


def remove_heading_numbering(document):
    for style in document.styles:
        if style.type == WD_STYLE_TYPE.PARAGRAPH and style.name.startswith("Heading"):
            ppr = style.element.pPr
            if ppr is not None:
                for numpr in ppr.findall(qn("w:numPr")):
                    ppr.remove(numpr)


def add_missing_styles(document):
    names = {s.name for s in document.styles}
    for name, base in PARAGRAPH_STYLES.items():
        if name in names:
            continue
        style = document.styles.add_style(name, WD_STYLE_TYPE.PARAGRAPH)
        if base in {s.name for s in document.styles}:
            style.base_style = document.styles[base]
        names.add(name)
    for name in CHARACTER_STYLES:
        if name not in names:
            document.styles.add_style(name, WD_STYLE_TYPE.CHARACTER)
            names.add(name)
    if "Table" not in names:
        table = document.styles.add_style("Table", WD_STYLE_TYPE.TABLE)
        table.base_style = document.styles["Table Grid"]


def keep_first_paragraph_flush(document):
    """Pandoc's first paragraph after a heading keeps the thesis indent."""
    style = document.styles["First Paragraph"]
    style.paragraph_format.first_line_indent = document.styles["Normal"].paragraph_format.first_line_indent


def caption_no_indent(document):
    for name in ("Image Caption", "Table Caption"):
        document.styles[name].paragraph_format.first_line_indent = 0


def toc_heading_outside_outline(document):
    """The contents title must not list itself in the table of contents."""
    style = document.styles["TOC Heading"]
    ppr = style.element.get_or_add_pPr()
    for lvl in ppr.findall(qn("w:outlineLvl")):
        ppr.remove(lvl)
    lvl = ppr.makeelement(qn("w:outlineLvl"), {qn("w:val"): "9"})
    ppr.append(lvl)


def add_title_page_style(document):
    """Centred paragraphs of the cover pages (used through custom-style)."""
    from docx.enum.text import WD_ALIGN_PARAGRAPH

    if "Title Page" in {s.name for s in document.styles}:
        return
    style = document.styles.add_style("Title Page", WD_STYLE_TYPE.PARAGRAPH)
    style.base_style = document.styles["Normal"]
    style.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.CENTER
    style.paragraph_format.first_line_indent = 0


def main(source: str) -> None:
    document = Document(source)
    clear_body(document)
    remove_heading_numbering(document)
    add_missing_styles(document)
    keep_first_paragraph_flush(document)
    caption_no_indent(document)
    add_title_page_style(document)
    toc_heading_outside_outline(document)
    OUT.parent.mkdir(parents=True, exist_ok=True)
    document.save(OUT)
    print(f"wrote {OUT}")


if __name__ == "__main__":
    if len(sys.argv) != 2:
        sys.exit(__doc__)
    main(sys.argv[1])
