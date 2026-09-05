# Notes on the reproduction

This file records what was learned while rebuilding the thesis from its
original notebooks (git tag `legacy-2024`) into the pipeline of this
repository. It lists the choices made to reproduce the submitted thesis
exactly, the places where the submitted thesis is internally inconsistent, and
the small deviations that could not be removed.

## Choices that reproduce the submitted results

* **Two calibrations of the SELA initiatives budget.** The thesis (section
  3.3.4) reports that the published total of the 2018 initiatives track,
  24,038,827 NIS, does not reproduce the approved grants and that an
  alternative total of 23,959,050 NIS was calibrated. The original code used
  two values: 23,907,075 NIS for the main analysis (Table 3, Table 4, Figures
  17 and 18, and the "214 of 255 authorities" statement) and 23,959,050 NIS
  for the sensitivity analysis (Figure 20). Only the first reproduces the
  tables; only the second reproduces Figure 20. Both are kept, in
  `sela_init_total()`, and the text of section 3.3.4 is left as submitted.
* **Likud vote share rounded to one decimal.** The coefficients of model M2 in
  Table 4 (intercept 334,921; 1,854 NIS per percentage point) are reproduced
  only when the Likud share of each authority is rounded to one decimal before
  fitting; with the unrounded share the intercept is 334,927 and the
  coefficient 1,855. The rounding is applied in `read_elections_2015()`.
* **Adjusted R-squared of M2.** With the rounded share the adjusted R-squared
  of M2 is 0.4154, which prints as 0.415; the submitted Table 4 prints 0.416
  (the value obtained with the unrounded share). The text's "6.3%" is the
  difference of the two printed adjusted R-squared values (0.416 - 0.353).
* **National priority localities.** The original code dropped the first row of
  each locality table of Government Decision 667 and applied the "50% of
  localities" rule to the table of individually listed localities rather than
  to the border/threatened table. Both are reproduced (`read_priority_localities()`,
  `read_national_priority()`); the alternative reading gives the same set of
  national priority authorities.
* **Peripherality index 2004.** Six authorities created after 2004 have no
  value; they take their 2015 value, as in the thesis.
* **Population weights.** Inequality measures weight each authority by its
  population rounded to a whole person. The original code truncated
  (`as.integer`), which lost one resident in a handful of small authorities;
  the difference does not reach the third decimal of any reported measure.
* **Skewness and kurtosis** follow the definitions of the `timeDate` package
  used originally (sample variance in the denominator), giving 0.23 and -0.93.

## Internal inconsistencies of the submitted thesis

These were found while verifying the reproduction. The thesis text is kept as
submitted; the author may wish to treat them as errata.

1. Section 3.3.4 quotes 23,959,050 NIS as the calibrated initiatives total,
   but Tables 3 and 4 and the "214 of 255" statement were computed with
   23,907,075 NIS (see above).
2. Table 4 labels the population coefficient "מספר תושבים ברשות (אלפים)", yet
   the coefficient (0.22 NIS in M1, 0.17 NIS in M2) is per resident, not per
   thousand residents.
3. Table 3 gives the standard deviation of the eligibility as 100,453 NIS; the
   text below Figure 17 gives 100,452 NIS. The computed value is 100,452.6.
4. Figure 20's caption reads "* p < 0.1"; the stars were computed with
   p < 0.05 (one star), p < 0.01 (two) and p < 0.001 (three).
5. Footnote 5 states that Mikveh Israel was classified under Holon. In the
   data the Council for Conservation of Heritage Sites (registered at Mikveh
   Israel, 29-42 million NIS a year) has no local authority and is excluded
   from every total, as it was in the submitted figures. The excluded amount
   is about 5-6% of the annual budget, not "less than 2%" as the footnote
   says of prizes to individuals (those alone are about 0.8%).
6. The formula for the hypothetical budget in section 3.2.4 multiplies where
   the text (and the code) divide: the share of an authority in the non-SELA
   budget is (B_i - B_sela,i) divided by the sum over authorities, then
   multiplied by the SELA total.

## Deviations that remain

* The Companies Registrar extract of 2022 was not preserved; the snapshot in
  `data/raw/organizations/companies_registry.csv.gz` was rebuilt in 2026 and
  places a few companies differently. The 2017 total in Figure 5 reads 698M
  rather than 699M, and the Gini coefficient differs by 0.001 in 2017 (0.687
  and 0.742 hypothetical, against 0.686 and 0.741) and 2018 (0.669 against
  0.668). Every other value in the figures and tables is reproduced.
* The histogram of Figure 17 splits one bin differently (the data of the
  submitted figure predate the final national-priority coding).
* Figures 1-4, 21 and 22 were drawn by hand in Word; they are reproduced as
  images cropped from the submitted PDF, not regenerated.
* The lists of figures and tables of the Word output are Word fields, filled
  in when the document is opened (Word asks to update fields).
* In the PDF, the footnote attached to the caption of Figure 1 is also
  printed under the list of figures, where Typst repeats the caption.
* Tables are set in 10 pt in the PDF and the Word file, as in the submitted
  thesis; Table 4 (the regression models) reads left to right in all formats,
  as in the submitted thesis, while the other tables read right to left.
