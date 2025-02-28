
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MCCPT

<!-- badges: start -->
<!-- badges: end -->

The **MCCPT** package conducts Monte-carlo changepoint analysis on
paleoclimate records. It is an implementation of Rebecca Killick’s
[changepoint method](https://github.com/rkillick/changepoint/), but
applied to paleoclimate records whilst accounting for age-model
uncertainty.

If you have questions or comments, you can contact the package
maintainers:

- [Haidee Cadd](https://github.com/h-cadd/) \| <haidee@uow.edu.au>
- [Matt Harris](https://github.com/MRPHarris/) \| <m.harris@gns.cri.nz>

## Using the package

### Installation

Install **MCCPT** with the devtools package:
`devtools::install_github("h-cadd/MCCPT", build = FALSE)`

### Using your data

Data must be structured in a specific way in order to be used with the
package. **MCCPT** currently accepts .xlsx files with the following (on
separate sheets):

1.  ‘Metadata’, containing the entries ‘Site code’ (an abbreviation),
    and ‘Data type’ (i.e., Compositional or Single). Here is an example
    from the data included with the package:

| category                         | value                            |
|:---------------------------------|:---------------------------------|
| Site name                        | Native Companion Lagoon          |
| Site code                        | NCL                              |
| Record length (cm)               | 388                              |
| Latitude                         | 27°40’48’’S                      |
| Longitude                        | 153°24’36’’E                     |
| m’s abovel sea level             | 20 m a.s.l                       |
| Current dominant vegetation type | Open eucalypt woodland.          |
| Geology                          | Sand.                            |
| Temperature min & max            | mean min = 19ᵒC, mean max = 25ᵒC |
| Mean annual rainfall             | 1514 mm pa                       |
| Age of record (sampled, year AD) | 2017                             |
| Age of record (base, cal yrs BP) | 44000                            |
| Dating method                    | Radiocarbon (14C)                |
| Analysed Proxies                 | Pollen                           |
| “Best” Hydrological proxy        | NA                               |
| “Best” Temp proxy                | Pollen                           |
| Hiatus/boundary                  | NA                               |
| Data type                        | Compositional                    |

2.  ‘Data’, containing a formatted data frame of the data you are
    interested in. This must have at least two columns:

- Depth (exact column name may vary but it must contain the
  case-sensitive string ‘Depth’)
- Any number of other columns containing proxy data (pollen species,
  d18O, etc.). This will be compressed into a principal curve.

3.  ‘Age_iterations’, containing age model iterations of the proxy
    record at the same interval resolution as the proxy data.

Refer to the example data contained in the package
(MCCPT/data-raw/Stradbroke-comp-raw/), derived from [Cadd et
al. (2024)](https://onlinelibrary.wiley.com/doi/10.1002/jqs.3681?af=R).

### Running MCCPT

Once you have installed the package and formatted your data
appropriately, run `conduct_MCCPT()`. This will generate:

- an R list of per-record changepoints, depending on your choices made
  whilst the program is running.
- an excel spreadsheet for each record, containing sheets corresponding
  to data for each changepoint.
- plots of each record, the position of changepoints, and their
  distribution within age model iterations.

### Attribution

**MCCPT** was developed originally for [Cadd et
al. (2021)](https://doi.org/10.1017/qua.2021.16).

<p align="center">
<img src="man/figures/Cadd2021_Title.JPG" height="300px" />
</p>

If you use the **MCCPT** package, please cite this paper. As **MCCPT**
relies heavily on the `changepoint` package, you should also cite
[Killick & Eckley.
(2021)](https://www.jstatsoft.org/article/view/v058i03).

An example citation might read something like: *… to identify shifts in
our records, we conducted a changepoint analysis (Killick & Eckley,
2014). We used the MCCPT R package, which applies a monte-carlo approach
to account for age uncertainty in the position of changepoints within
paleoclimate records (Cadd et al., 2021).*

## References

Cadd, H., Petherick, L., Tyler, J., Herbert, A., Cohen, T. J.,
Sniderman, K., … Harris, M. R. P. (2021). A continental perspective on
the timing of environmental change during the last glacial stage in
Australia. *Quaternary Research*, 102, 5–23.
[doi:10.1017/qua.2021.16](https://doi.org/10.1017/qua.2021.16)

Killick, R. & Eckley, I. A. (2014). changepoint: An R Package for
Changepoint Analysis. *Journal of Statistical Software*, 58 (3), 1-19.
[doi:10.18637/jss.v058.i03](https://doi.org/10.18637/jss.v058.i03)
