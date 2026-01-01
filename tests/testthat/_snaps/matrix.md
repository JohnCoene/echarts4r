# e_matrix fails informatively without xAxis or yAxis

    Code
      e_matrix(e, xAxis = "Class")
    Condition
      Error:
      ! must provide both x and y values

---

    Code
      e_matrix(e, yAxis = "Grade")
    Condition
      Error:
      ! must provide both x and y values

# e_matrix_raw fails informatively without rows or cols

    Code
      e_matrix_raw(rows = 3)
    Condition
      Error in `e_matrix_raw()`:
      ! Must provide both the number of rows and columns.

---

    Code
      e_matrix_raw(cols = 3)
    Condition
      Error in `e_matrix_raw()`:
      ! Must provide both the number of rows and columns.

---

    Code
      e_matrix_raw()
    Condition
      Error in `e_matrix_raw()`:
      ! Must provide both the number of rows and columns.

# e_geoFacet errors informatively with invalid grid type

    Code
      e_geoFacet(e, grid = 123)
    Condition
      Error:
      ! must provide valid grid. Either name of geofacet grid object or custom dataframe grid

# e_pie_matrix errors informatively without matrix coordinate system

    Code
      e_pie_matrix(e, x = "A", y = "B")
    Condition
      Error in `e_pie_matrix()`:
      ! Matrix coordinate system must be built before adding data. e.g. e_matrix()

# e_scatter_matrix errors informatively without matrix coordinate system

    Code
      e_scatter_matrix(e, z = "A")
    Condition
      Error in `e_scatter_matrix()`:
      ! Matrix coordinate system must be built before adding data. e.g. e_matrix()

# e_heatmap_matrix errors informatively without matrix coordinate system

    Code
      e_heatmap_matrix(e, z = "A")
    Condition
      Error in `e_heatmap_matrix()`:
      ! Matrix coordinate system must be built before adding data. e.g. e_matrix()

