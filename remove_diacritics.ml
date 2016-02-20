#load "str.cma";;

let remove_diacritics str =
  let table = [
    ("a", ["ã"; "á"; "à"; "â"]);
    ("e", ["é"; "è"; "ê"]);
    ("i", ["í"; "ì"; "î"]);
    ("o", ["ó"; "ò"; "ô"]);
    ("u", ["ú"; "ù"]);
    ("n", ["ñ"]);
    ("A", ["Ã"; "Á"; "À"; "Â"]);
    ("E", ["É"; "È"; "Ê"]);
    ("I", ["Í"; "Ì"; "Î"]);
    ("O", ["Ó"; "Ò"; "Ô"]);
    ("U", ["Ú"; "Ù"]);
    ("N", ["Ñ"])
  ] in
  let rec replace str replacement characters = match characters with
    | [] -> str
    | character :: tail ->
      replace
        (Str.global_replace (Str.regexp_string character) replacement str)
        replacement
        tail
  in
  let rec remove str map =
    match map with
      | [] -> str
      | (_, [])::_ -> str
      | (replacement, characters)::t ->
        remove (replace str replacement characters) t
      in
  remove str table
  ;;
