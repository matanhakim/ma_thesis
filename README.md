<div dir="rtl">

# "דיבידנד הנאמנות בתרבות": צמצום אי-שוויון במימון תרבות ככלי לבניית נאמנות פוליטית

**מתן חכים** | עבודת גמר מחקרית (תזה) לתואר מוסמך, החוג לסוציולוגיה, אוניברסיטת חיפה, יולי 2024 | בהנחיית פרופ' טלי כץ-גרו

[![Reproduce the thesis](https://github.com/matanhakim/ma_thesis/actions/workflows/reproduce.yml/badge.svg)](https://github.com/matanhakim/ma_thesis/actions/workflows/reproduce.yml)

מאגר זה מכיל את הנתונים, הקוד והטקסט של התזה במלואם, ערוכים כך שכל אדם יוכל להריץ את הניתוח מתחילתו ועד סופו ולקבל את התזה עצמה, על כל איוריה וטבלאותיה, בשלושה פורמטים: HTML, Word ו-PDF. המאגר נבנה כך שגם בעוד שנים, על מחשב אחר ועם גרסאות אחרות של התוכנות, התוצאות יהיו זהות.

## על המחקר

העבודה בוחנת את היקפו של אי-השוויון בין רשויות מקומיות בישראל במימון תרבות בשנים 2013-2019, ואת מקומה של תקנת "סל תרבות עירוני" (סל"ע) בצמצומו. הממצאים המרכזיים:

- אי-השוויון במימון תרבות בין רשויות מקומיות הצטמצם בשנים 2016-2019 לעומת 2013-2015, בכל החתכים שנבחנו: מגזר, סוג רשות, אשכול חברתי-כלכלי, אשכול פריפריאליות ומדדי אי-שוויון כוללים (מדד ג'יני וחלוקה לעשירוני מימון).
- הסיבה המרכזית לצמצום היא תקנת סל"ע. חלוקה היפותטית של תקציב התקנה לפי ההתפלגות של שאר תקציב התרבות הייתה משאירה את אי-השוויון גבוה יותר.
- גובה הזכאות של רשות מקומית לתקנת סל"ע בשנת 2018 קשור באופן מובהק לאחוז ההצבעה לליכוד בבחירות 2015 ברשויות עם רוב יהודי: כל אחוז הצבעה נוסף לליכוד הגדיל את הזכאות בכ-1,854 ש"ח, בניכוי מספר התושבים, האשכול החברתי-כלכלי ואשכול הפריפריאליות. זהו "דיבידנד הנאמנות בתרבות".

## מבנה המאגר

| נתיב | תוכן |
|---|---|
| `thesis.qmd` | מסמך ה-Quarto של התזה: הטקסט המלא, וקטעי קוד קצרים שמציירים את האיורים והטבלאות מתוצרי הפייפליין |
| `_targets.R` | הפייפליין של הניתוח (חבילת `targets`): קריאת כל קבצי הנתונים, בניית פאנל של 255 רשויות מקומיות לשנים 2013-2019, מדדי אי-השוויון, חישוב הזכאות לסל"ע, מודלי הרגרסיה, ובסופו רינדור התזה |
| `R/` | הפונקציות של הניתוח, קובץ לכל שלב: קריאת נתוני הלמ"ס (`read_cbs.R`), בחירות (`read_elections.R`), ארגונים (`read_organizations.R`), תקציבים (`read_budget.R`), אזורי עדיפות לאומית (`national_priority.R`), בניית הפאנל (`build_panel.R`), מדדי אי-שוויון (`inequality.R`), תקנת סל"ע והמודלים (`sela.R`), האיורים (`plots.R`) והטבלאות (`tables.R`). כל פונקציה מתועדת |
| `data/raw/` | קבצי המקור כפי שהתקבלו ממפרסמיהם (למ"ס, ועדת הבחירות, מפתח התקציב, משרד התרבות, גיידסטאר, רשם החברות, החלטת ממשלה 667). ראו `data/README.md` למקור, לתאריך ולשימוש של כל קובץ |
| `data/reference/` | טבלאות עזר: מזהי הרשויות המקומיות בשלוש מערכות (למ"ס, משרד החינוך, רשות המסים), רשימת כל הכתיבים של שמות יישובים, ושיוך ידני של ארגונים שאינם ברשמים |
| `data-raw/` | הסקריפט שיצר את תמונת-המצב של רשם החברות מפורטל data.gov.il |
| `figures/static/` | ששת האיורים הרעיוניים (1-4, 21, 22) שצוירו ידנית ב-Word, כתמונות מהתזה שהוגשה |
| `tests/` | בדיקות יחידה (`testthat`) לפונקציות המרכזיות ובדיקות שלמות לפייפליין, כולל השוואה של הממצאים המרכזיים למספרים שבתזה |
| `scripts/` | כלי עזר: השוואת התזה המרונדרת לתזה שהוגשה, עיבוד-אחר של קובץ ה-Word, בניית תבנית ה-Word, חילוץ האיורים הסטטיים |
| `assets/`, `filters/` | תבנית Word (נגזרת מהתזה שהוגשה), תבנית Typst ל-PDF, CSS ל-HTML ומסנני Lua קצרים לעיצוב |
| `reference/` | התזה כפי שהוגשה בנובמבר 2024 (PDF ו-Word), לצורך ההשוואה |
| `renv.lock`, `Dockerfile` | הקפאת סביבת התוכנה: גרסת R, כל חבילות R ומקורותיהן, Quarto והגופנים |

## איך להריץ

ראו את הפרק [Quick start](#quick-start) באנגלית להלן. בקצרה: `targets::tar_make()` מריץ את כל הניתוח ומרנדר את התזה אל התיקייה `output/`. הדרך המהימנה ביותר היא באמצעות Docker, שמבטיח סביבה זהה לזו שבה נבנה המאגר.

## איך הניתוח בנוי

הפייפליין מתחיל מרשימה קבועה של 255 הרשויות המקומיות שהיו קיימות בשנים 2013-2019 ומצרף לכל רשות ולכל שנה: את תקציב התמיכות של מינהל התרבות שהגיע לארגונים הרשומים בשטחה (לפי הרשם המתאים לכל ארגון: עמותות בגיידסטאר, חברות ברשם החברות, והרשויות עצמן), את תקציב תקנת סל"ע, את האוכלוסייה, את המגזר (יהודי או ערבי), את סוג הרשות, את האשכולות החברתיים-כלכליים והפריפריאליים של הלמ"ס, את סיווגה כאזור עדיפות לאומית ואת תוצאות בחירות 2015. מן הפאנל הזה מחושבים מדדי אי-השוויון (פרק 4.1), התקציב ההיפותטי ללא סל"ע (פרק 4.2), והזכאות של כל רשות לתקנת סל"ע בשנת 2018 לפי מבחני התמיכה, המשמשת כמשתנה התלוי בשני מודלי הרגרסיה ובניתוח הרגישות (פרק 4.3).

הפקודה `targets::tar_visnetwork()` מציגה את גרף התלויות המלא, ו-`targets::tar_read(panel)` מחזירה את הפאנל עצמו לעיון.

## נאמנות לתזה שהוגשה

הסקריפט `scripts/compare_with_submitted.R` משווה את הטקסט של התזה המרונדרת לטקסט של התזה שהוגשה ומפיק דוח (`output/comparison_report.md`). בבנייה האחרונה הדמיון היה 99.6% בין קובצי ה-Word ו-99.2% בין קובצי ה-PDF, ו-230 מתוך 231 הפסקות של התזה שהוגשה מופיעות בקובץ המרונדר מילה במילה (הפסקה החסרה היא הטקסט שבתוך תרשים 3, שמופיע כאן כתמונה). כל האיורים והטבלאות משוחזרים במספריהם, פרט לשלושה פרטים שאינם ניתנים לשחזור מדויק מפני שקובץ רשם החברות המקורי משנת 2022 לא נשמר ותמונת-המצב הנוכחית שונה ממנו במעט: תווית התקציב הכולל לשנת 2017 באיור 5 (698 מיליון ש"ח במקום 699), ומדד ג'יני בשנים 2017 ו-2018 (סטייה של 0.001). ראו `data/README.md`.

בעבודת השחזור התגלו כמה אי-התאמות פנימיות בתזה שהוגשה, שנשמרו כאן כפי שהן כדי שהתוצרים יתאמו לתזה, ומתועדות בקובץ `NOTES.md`.

## רישיון

הקוד והתיעוד משוחררים ברישיון MIT (ראו `LICENSE`). טקסט התזה הוא יצירתו של מתן חכים, כל הזכויות שמורות. קבצי הנתונים הגולמיים הם קניינם של מפרסמיהם ומופצים כאן לצורך שחזור המחקר בלבד; ראו `data/README.md`.

## ציטוט

חכים, מ. (2024). *"דיבידנד הנאמנות בתרבות": צמצום אי-שוויון במימון תרבות ככלי לבניית נאמנות פוליטית* [עבודת גמר לתואר מוסמך, אוניברסיטת חיפה]. https://github.com/matanhakim/ma_thesis

</div>

---

## Quick start

The repository rebuilds the thesis from the raw data files in `data/`, with the
R package [`targets`](https://docs.ropensci.org/targets/) orchestrating the
analysis and [Quarto](https://quarto.org) rendering the document to HTML, Word
and PDF. Every R package is pinned in `renv.lock`; the `Dockerfile` pins R,
Quarto and the fonts as well.

### With Docker (recommended)

```bash
git clone https://github.com/matanhakim/ma_thesis.git
cd ma_thesis
docker build -t ma-thesis .
docker run --rm -v "$PWD/output:/thesis/output" ma-thesis
```

The three rendered files, `output/thesis.html`, `output/thesis.docx` and
`output/thesis.pdf`, appear after a few minutes. The GitHub Actions workflow
(`.github/workflows/reproduce.yml`) runs exactly these steps on every push and
publishes the results as build artifacts.

### On your own machine

Requirements: R 4.5, [Quarto](https://quarto.org/docs/get-started/) 1.8 or
later, and the David typeface (installed with Windows; on Linux install the
Culmus fonts, which provide David CLM).

```r
install.packages("renv")
renv::restore()          # installs the exact package versions of renv.lock
targets::tar_make()      # runs the analysis and renders output/thesis.{html,docx,pdf}
```

Useful commands once the pipeline has run:

```r
targets::tar_visnetwork()                      # the dependency graph of the analysis
targets::tar_read(panel)                       # the 255 x 7 municipality-year panel
targets::tar_read(sela_models)$m2 |> summary() # the research model (M2)
source("tests/testthat.R")                     # unit and integration tests
Rscript scripts/compare_with_submitted.R       # text comparison with the submitted thesis
```

### Layout

```
thesis.qmd          the thesis: text, and short chunks that draw figures and tables
_quarto.yml         output formats (HTML, Word via a reference document, PDF via Typst)
_targets.R          the analysis pipeline, from raw files to rendered thesis
R/                  documented functions, one file per stage
data/raw/           source files as published (see data/README.md)
data/reference/     identifier and name lookup tables
tests/              testthat unit and integration tests
scripts/            comparison and post-processing tools
assets/, filters/   Word and Typst templates, CSS, Lua filters
reference/          the submitted thesis (PDF and Word) for comparison
```
