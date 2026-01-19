# errors informatively without annotations

    Code
      e_annotations(e_charts(mtcars, mpg))
    Condition
      Error in `e_annotations()`:
      ! must provide list of annotations

# errors informatively with non-list annotations

    Code
      e_annotations(e_charts(mtcars, mpg), "not a list")
    Condition
      Error in `e_annotations()`:
      ! annotations must be a list

---

    Code
      e_annotations(e_charts(mtcars, mpg), list("not nested"))
    Condition
      Error in `e_annotations()`:
      ! annotations must be a list

# process_single_annotation errors informatively without required fields

    Code
      process_single_annotation(list(x = 1, y = 2))
    Condition
      Error in `process_single_annotation()`:
      ! Annotation must have x, y, and text fields

---

    Code
      process_single_annotation(list(x = 1, text = "test"))
    Condition
      Error in `process_single_annotation()`:
      ! Annotation must have x, y, and text fields

---

    Code
      process_single_annotation(list(y = 2, text = "test"))
    Condition
      Error in `process_single_annotation()`:
      ! Annotation must have x, y, and text fields

