data sourced from repurrrsive package and wrangled with R studio


got_aliases:
(one::many, has missing values)
id    alias
sample question: which character has the most aliases?


got_books:
id    book
(one::many, no missing values)
sample question: which character appears in the most books?

got_char:
id    name    culture    gender    born    alive
(one::one, has missing values)
sample question: how many characters are alive?
sample question: what is the gender distribution of characters?

got_house:
(one::many, missing values = "NA")
id    house
sample question: what house has the most members?
sample question: what character belongs to more than one house?
sample question: which character belongs to the most houses?

got_series:
(one::many, has missing values)
id    season
sample question: which character appears in the most seasons?

got_titles (official character titles):
(one::many, has missing values)
id    title
sample question: which character has the most titles?