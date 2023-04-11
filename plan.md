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
  * Upload CSV
  * Choose import profile
  * Validate parsing is fine
  * import
  * categorize

Arch
  * Try a client side only implementation without server
    * split elm state into UI state and data
    * action to serialize, action to deserialize.
    * using elm-serialize
