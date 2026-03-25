# HW 5 Lexing and Tokenizing

## By John Sayles and Tenzin Sommer

## Test Cases:

### Consume String Literal

* string with no trailing whitespace
    * done
* string with trailing whitespace outside
    * done
* string with trailing whitespace inside
    * done
* string with chars before it
    * done
* string with chars after it
    * done
* treat number as string
    * done
* no opening quote
    * done
* extra internal quotes
    * done
* no ending quote
    * done

### Consume Keyword

* keyword by itself
    * done
* keyword with extra chars behind it
    * done
* keyword with extra chars ahead of it
    * done
* more than one keyword in string
    * done

### Tokenize

* simple object
    * done
* Object inside object
    * done
* list inside object
    * done
* Empty JSON object
    * done
* List inside list
    * done
* Malformed JSON object
    * done
* Unknown chars
    * done
