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

