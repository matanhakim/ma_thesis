"""Extract the six conceptual diagrams of the thesis from the submitted PDF.

Figures 1-4, 21 and 22 were drawn by hand in Word (SmartArt and shapes), not
generated from data. They are reproduced as images cropped from the submitted
thesis (reference/thesis_submitted_2024.pdf) at 300 dpi.

Usage: python scripts/extract_static_figures.py
"""
from pathlib import Path

import fitz  # PyMuPDF

ROOT = Path(__file__).resolve().parents[1]
PDF = ROOT / "reference" / "thesis_submitted_2024.pdf"
OUT = ROOT / "figures" / "static"

# (output name, 1-based PDF page, clip rectangle in PDF points)
FIGURES = [
    ("fig01_local_government_structure.png", 10, (178, 407, 506, 707)),
    ("fig02_central_government_pattern.png", 17, (85, 98, 491, 354)),
    ("fig03_four_funding_routes.png", 19, (168, 436, 406, 622)),
    ("fig04_literature_claims.png", 25, (87, 104, 523, 347)),
    ("fig21_main_findings.png", 59, (67, 122, 503, 330)),
    ("fig22_culture_administration_pattern.png", 62, (85, 204, 491, 460)),
]


def main() -> None:
    OUT.mkdir(parents=True, exist_ok=True)
    doc = fitz.open(PDF)
    for name, page_no, clip in FIGURES:
        page = doc[page_no - 1]
        pix = page.get_pixmap(dpi=300, clip=fitz.Rect(*clip))
        pix.save(OUT / name)
        print(f"{name}: page {page_no}, {pix.width}x{pix.height} px")


if __name__ == "__main__":
    main()
