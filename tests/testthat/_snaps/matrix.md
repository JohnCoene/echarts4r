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

