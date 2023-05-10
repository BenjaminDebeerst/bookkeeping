x site mit eingabe: zeile
x trennen bei komma
x anzeige in tabelle

* import profile:
  * define columns to import
    * split char = ","
    * always trim
    * type = date | text | number
    * col-no -> type
    * combination columns: this or (-)that, this * that
  * date and number parsing

Flow
  x Upload CSV
  * Choose import profile
  x Validate parsing is fine
  x import
  * categorize

Arch
  x Try a client side only implementation without server
    x split elm state into UI state and data
    x action to serialize, action to deserialize.
    x using elm-serialize
  x Don't store computed values
    * Only store the raw csv string + hash identifier, for each hash: parsing mappings and compute instructions

How to categorize
  * "Book" is a generic table view
  * Useful filters at the top: Month/Year (time window?), category, amount, description
  * Useful stats at the top: #entries, sum, avg?
  * Filter for "uncategorized"
  * Button to switch to category-edit mode
    * Keyboard-only navigation allows for easy edit of category
    * A category has a name and (optionally) a (user-defined) text code - this is what is expected in the input field
    * The category found is displayed inline, if none of the given code is found, display a hint that allows to create a category