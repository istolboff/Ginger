Feature: Lemma Versions Disambiguating Annotations
    В тех случаях когда в паттерне есть слова, которые при парсинге имеют несколько возможных вариантов Lemma Versions,
    нужно возможность указать прямо в тексте паттерна, какую именно версию следует использовать.

Scenario: Lemma Versions Disambiguating Annotation
    Then the following variants should be proposed by the disambiguation API
        | Sentence with ambiguous lemma versions                         | Proposed disambiguation                |
        | если имеется свободный слот, то ЕГО занимает женщина           | (вин.,муж.); (вин.,ср.); (род.,ср.)    |
     And disambiguation annotation should be applied correctly
        | Sentence with disambiguation annotation                        | Ambiguous word  | Parsed Grammar Characteristics                                |
        | если имеется свободный слот, то ЕГО(вин.,ср.) занимает женщина | ЕГО             | PronounCharacteristics { Case: Винительный, Gender: Средний } |


Scenario: A few examples of Text Markup
    Then the following text markups should be parsed correctly
        | Text                                                           | Parsing Results                        |
        |----------------------------------------------------------------|----------------------------------------|
        | победитель получает все                                        |  победитель false null []              |
        |                                                                |  получает false null []                |
        |                                                                |  все false null []                     |
        |----------------------------------------------------------------|----------------------------------------|
        | заяц переходит с ~одного~ размера куста{мн.} на ~другой~       |  заяц false null []                    |
        |                                                                |  переходит false null []               |
        |                                                                |  с false null []                       |
        |                                                                |  одного true null []                   |
        |                                                                |  размера false null []                 |
        |                                                                |  куста false PluralitySensitive []     |
        |                                                                |  на false null []                      |
        |                                                                |  другой true null []                   |
        |----------------------------------------------------------------|----------------------------------------|
        | ~если имеется{мн.}~ свободный{мн.} слот{мн.}, то его{мн.}      |  если true null []                     |
        | занимает{мн.} женщина {и проч.}                                |  имеется true PluralitySensitive []    |
        |                                                                |  свободный false PluralitySensitive [] |
        |                                                                |  слот false PluralitySensitive []      |
        |                                                                |  , false null []                       |
        |                                                                |  то false null []                      |
        |                                                                |  его false PluralitySensitive []       |
        |                                                                |  занимает false PluralitySensitive []  |
        |                                                                |  женщина false Replicatable []         |
        |----------------------------------------------------------------|----------------------------------------|
        | стол{мн.} ~существует{мн.} - его(вин.,муж.) {мн.}              |  стол false PluralitySensitive []      |
        |  должно быть~ видно                                            |  существует true PluralitySensitive [] |
        |                                                                |  - true  null []                       |
        |                                                                |  его true PluralitySensitive [вин,муж] |
        |                                                                |  должно true null []                   |
        |                                                                |  быть true null []                     |
        |                                                                |  видно false null []                   |
        |----------------------------------------------------------------|----------------------------------------|
