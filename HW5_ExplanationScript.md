# HW 5 Lexing and Tokenizing

## By John Sayles and Tenzin Sommer

## Test Cases:

### Consume String Literal

* string with no trailing whitespace
    * done
* string with trailing whitespace
* string with chars before it
* string with chars after it
    * done
* treat number as string
    * done
* string with special characters separating 2 parts
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
* more than one keyword in string
    * done

### Tokenize

* simple object
    * done
* Object inside object
    * done
* Object inside list inside object
* Empty JSON object
* List inside list
* Malformed JSON object
