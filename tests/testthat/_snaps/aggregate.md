# aggregating from glycoforms to glycopeptides fails

    Code
      aggregate(exp, to_level = "gp")
    Condition
      Error in `aggregate()`:
      ! Cannot aggregate from "gf" to "gp".
      i See `?aggregate` for more details.

# aggregating from glycoforms without structures to glycoforms with structures fails

    Code
      aggregate(exp, to_level = "gfs")
    Condition
      Error in `aggregate()`:
      ! Cannot aggregate from "gf" to "gfs".
      i See `?aggregate` for more details.

