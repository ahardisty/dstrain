data sourced from repurrrsive package and wrangled with R studio


got_aliases:
(one::many, has missing values)
id    alias
ANALYST sample question: which character has the most aliases?
CONSULTANT sample question: what is the name of the character with the most aliases?


got_books:
id    book
(one::many, no missing values)
ANALYST sample question: which character appears in the most books?

got_char:
id    name    culture    gender    born    alive
(one::one, has missing values)
ANALYST sample question: how many characters are alive?
ANALYST sample question: what is the gender distribution of characters?

got_house:
(one::many, missing values = "NA")
id    house
ANALYST sample question: what house has the most members?
ANALYST sample question: what character belongs to more than one house?
ANALYST sample question: which character belongs to the most houses?

got_series:
(one::many, has missing values)
id    season
ANALYST sample question: which character appears in the most seasons?
CONSULTANT sample question: which characters appear in TV series and NOT in books?
CONSULTANT sample question: which characters appear in books and NOT in tv series?
CONSULTANT sample question: which characters appear in the same number of books and tv series seasons?


got_titles (official character titles):
(one::many, has missing values)
id    title

ANALYST sample question: which character has the most titles?
CONSULTANT sample question: which character would you pick as an ally?
CONSULTANT sample question: which character would you pick as an enemy?