
### A repository for:

Fidino, M., Barnas, G.R., Lehrer, E.W., Murray, M., and Magle, S.B. (in prep). A trifold approach to quantify the effect of a common olfactory lure on the detection probability of mammals captured on camera traps

<div align="center"><img width="150" height="auto" src="./images/opossum.JPG" alt="A silhouette of an opossum." /></div>

<div align="center"> <h3>Scripts</h3> </div>

---

**This repository has 3 R Scripts used for the 3 seperate analyses. They will be in your working directory. These include:**

**fit_binomial_model.R:** This script reads in the detection / non-detection data of the species we could analyze and fits the binomal observation model to the data.

**fit_exponenttial_model.R:** This script reads in the time to first detection data of the species we could analyze and fits the exponential observaiton model to the data.

**fit_poisson_model.R:** This script reads in the number of images taken of the species we could analyze and fits the Poisson model to these data.


---

<div align="center"><img width="150" height="auto" src="./images/raccoon.JPG" alt="A silhouette of a raccoon." /></div>

<div align="center"> <h3>Models</h3> </div>

---

**This repository has 3 JAGS models that we used for our analysis. They should be placed within the jags_models sub-folder of the working directory. These include:**

**lure_occ_model.R:** The binomial detection model.

**time_to_event.R:** The exponential detection model.

**lure_occ_poisson.R:** the Poisson detection model.

---

<div align="center"><img width="150" height="auto" src="./images/coyote.JPG" alt="A silhouette of a coyote." /></div>

<div align="center"> <h3>Data</h3> </div>

---

**There are 2 data files within the `data` sub-folder which are used in this analysis. They include:**

**lure_position.csv:** This csv has 5 columns and 40 rows (excluding the header) and contains information on whether a lure was placed in view of a given camera.

| Column header | Data type | Description |
|---|---|---|
| `site`| Character | The site abbreviation. Sampling units are split apart by cameras A and B. |
| `week_1` | Numeric | Whether lure was present or not during the first week. 1 = lure, 0 = no lure control. |
| `week_2` | Numeric | Whether lure was present or not during the second week. 1 = lure, 0 = no lure control. |
| `week_3` | Numeric | Whether lure was present or not during the third week. 1 = lure, 0 = no lure control. |
| `week_4` | Numeric | Whether lure was present or not during the fourth week. 1 = lure, 0 = no lure control. |

<br>
<br>

**precip.csv:** This csv has 4 columns and 40 rows (excluding the header) and contains information on the number of inches of rain that occured over a given week of sampling. This csv is ordered identically to `lure_position.csv`

| Column header | Data type | Description |
|---|---|---|
| `week_1` | Numeric | Inches of rain on week 1 |
| `week_2` | Numeric | Inches of rain on week 2 |
| `week_3` | Numeric | Inches of rain on week 3 |
| `week_4` | Numeric | Inches of rain on week 4 |

 
 Migatory status for all species was compiled from https://www.allaboutbirds.org/
 
<br>
<br>
 


<div align="center"><img width="150" height="auto" src="./images/squirrel.JPG" alt="A silhouette of a squirrel." /></div>
