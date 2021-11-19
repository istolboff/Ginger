Feature: Sentence Understanding Functionality

Scenario: Каждый человек смертен
    Все слова в паттерне, которые не использованы в meaning, должны присутствовать в любом распознаваемом этим паттерном предложении.
    Given the following Patterns
    | Id | Pattern                                                   | Meaning                                                                      |
    | 1  | Каждый человек смертен                                    | смертный(X) :- человек(X).                                                   |
    | 2  | Любая сфера всегда имеет внешнюю и внутреннюю поверхности | поверхность(X, внешний) :- сфера(X). поверхность(X, внутренний) :- сфера(X). |
    Then the following understandings should be possible
    | Sentence                                      | Meaning                                                  | Recognized with Pattern |
    | Каждая книга конечна                          | конечный(X) :- книга(X).                                 | 1                       |
    | Любая река всегда имеет левый и правый берега | берег(X, левый) :- река(X). берег(X, правый) :- река(X). | 2                       |
    And the following sentences should fail to be understood
    | Sentence                                      |
    | Любой человек смертен                         |
    | Любая река иногда имеет левый и правый берега | 


Scenario: медведь лежит на одной стороне улицы
    Given the following Patterns
    | Id | Pattern                              | Meaning                                                 |
    | 1  | медведь лежит на одной стороне улицы | лежать(медведь,ОдинСторона), сторона(улица,ОдинСторона) |
    Then the following understandings should be possible
    | Sentence                                  | Meaning                                                 |
    | фермер находится на одном береге реки     | находиться(фермер, ОдинБерег), берег(река, ОдинБерег)   |

Scenario: Сфера ~имеет~ внешнюю и внутреннюю поверхности
    Те слова в паттерне, которые должны присутствовать в любом распознаваемом этим паттерном предложении, 
    и при этом используются в meaning, должны быть аннотированы символом ~
    Given the following Patterns
    | Id | Pattern                                          | Meaning                                                                    |
    # Cимволом ~ аннтоированы отдельные слова
    | 1  | Сфера ~имеет~ внешнюю и внутреннюю поверхности   | иметь(сфера, поверхность, внешний). иметь(сфера, поверхность, внутренний). |
    # Cимволом ~ аннтоированы последовательности слов
    | 2  | ~В исходном состоянии~ лебедь находится на сфере | исходныйСостояние([находиться(лебедь, на, сфера)]).                        |
    Then the following understandings should be possible
    | Sentence                                  | Meaning                                                      | Recognized with Pattern |
    | Мартышка имеет левую и правую руки        | иметь(мартышка, рука, левый). иметь(мартышка, рука, правый). | 1                       |
    | В исходном состоянии фермер сидит в лодке | исходныйСостояние([сидеть(фермер, в, лодка)]).               | 2                       |
    And the following sentences should fail to be understood
    | Sentence                                        |
    | Мартышка ест зеленый и желтый бананы            |
    | В начальном состоянии лебедь находится на сфере |


Scenario: Река имеет левый и правый берега
    Given the following Patterns
    | Id | Pattern                                      | Meaning                                                      |
    | 1  | Сфера имеет внешнюю и внутреннюю поверхности | поверхность(сфера, внешний). поверхность(сфера, внутренний). |
    Then the following understandings should be possible
    | Sentence                                      | Meaning                                                      | Recognized with Pattern |
    | Сфера имеет внешнюю и внутреннюю поверхности  | поверхность(сфера, внешний). поверхность(сфера, внутренний). | 1                       |
    | река имеет левый и правый берега              | берег(река, левый). берег(река, правый).                     | 1                       |
    | человек имеет физическую и умственную энергии | энергия(человек, физический). энергия(человек, умственный).  | 1                       |
    And the following sentences should fail to be understood
    | Sentence                                          |
    | Куб всегда имеет внешнюю и внутреннюю поверхности |
    | Сфера имеет внешнюю поверхность                   |
    | Сферы имеют внешнюю и внутреннюю поверхности      |


Scenario: Волк, коза и капуста являются перевозимыми существами
    Given the following Patterns
    | Id | Pattern                                            | Meaning                                                                        |
    | 3  | Лебедь, рак и щука являются отрицательными героями | отрицательныйГерой(лебедь). отрицательныйГерой(рак). отрицательныйГерой(щука). |
    Then the following understandings should be possible
    | Sentence                                              | Meaning                                                                             | Recognized with Pattern |
    | Волк, коза и капуста являются перевозимыми существами | перевозимыйСущество(волк). перевозимыйСущество(коза). перевозимыйСущество(капуста). | 3                       |
    And the following sentences should fail to be understood
    | Sentence                                            |
    | Волк, коза и капуста являются перевозимым существом |


Scenario: если волк, коза и капуста находятся на правом берегу реки, то миссия заканчивается успехом
    Given the following Patterns
    | Id | Pattern                                         | Meaning                         |
    |----|-------------------------------------------------|---------------------------------|
    | 4  | если лебедь, рак и щука находятся на внутренней | подаваться(сигнал, Z) :-        |
    |    | поверхности сферы, то сигнал подается звонком   |    находиться(лебедь, на, X),   |
    |    |                                                 |    находиться(рак, на, X),      |
    |    |                                                 |    находиться(щука, на, X),     |
    |    |                                                 |    поверхность(сфера, X),       |
    |    |                                                 |    X = внутренний,              |
    |    |                                                 |    Z = звонок.                  |
    |----|-------------------------------------------------|---------------------------------|
    Then the following understandings should be possible
    | Sentence                                             | Meaning                         | Recognized with Pattern |
    |------------------------------------------------------|---------------------------------|-------------------------|
    | Если волк, коза и капуста находятся на правом берегу | заканчиваться(миссия, Z) :-     | 4                       |
    | реки, то миссия заканчивается успехом                |   находиться(волк, на, X),      |                         |
    |                                                      |   находиться(коза, на, X),      |                         |
    |                                                      |   находиться(капуста, на, X),   |                         |
    |                                                      |   берег(река, X),               |                         |
    |                                                      |   X = правый,                   |                         |
    |                                                      |   Z = успех.                    |                         |
    |------------------------------------------------------|-------------------------------- |-------------------------|


Scenario: Если волк и коза находятся на одном берегу реки, а фермер находится на другом берегу реки, то миссия заканчивается неудачей с формулировкой 'волк съел козу'
    Given the following Patterns
    | Id | Pattern                                                    | Meaning                         |
    |----|------------------------------------------------------------|---------------------------------|
    | 5  | Если лебедь и рак ~находятся~ на одной поверхности кубика, | начинаться(басня, Z) :-         |
    |    | а щука находится на другой поверхности кубика, то          |   находиться(лебедь, на, X),    |
    |    | басня начинается молчанием по причине 'не так сели'        |   находиться(рак, на, X),       |
    |    |                                                            |   находиться(щука, на, Y),      |
    |    |                                                            |   поверхность(кубик, X),        |
    |    |                                                            |   поверхность(кубик, Y),        |
    |    |                                                            |   X \= Y,                       |
    |    |                                                            |   Z = молчание,                 |
    |    |                                                            |   причина(Z) = 'не так сели'.   |
    |----|------------------------------------------------------------|---------------------------------|
    Then the following understandings should be possible
    | Sentence                                             | Meaning                               | Recognized with Pattern |
    |------------------------------------------------------|---------------------------------------|-------------------------|
    | Если волк и коза находятся на одном береге реки,     | заканчиваться(миссия, Z) :-           |           5             |
    | а фермер находится на другом береге реки,            |    находиться(волк, на, X),           |                         |
    | то миссия заканчивается неудачей по причине          |    находиться(коза, на, X),           |                         |
    | 'волк съел козу'                                     |    находиться(фермер, на, Y),         |                         |
    |                                                      |    берег(река, X),                    |                         |
    |                                                      |    берег(река, Y),                    |                         |
    |                                                      |    X \= Y,                            |                         |
    |                                                      |    Z = неудача,                       |                         |
    |                                                      |    причина(Z) = 'волк съел козу'.     |                         |
    |------------------------------------------------------|---------------------------------------|-------------------------|
    | Если коза и капуста находятся на одном береге реки,  | заканчиваться(миссия, Z) :-           |           5             |
    | а фермер находится на другом береге реки,            |    находиться(коза, на, X),           |                         |
    | то миссия заканчивается неудачей по причине          |    находиться(капуста, на, X),        |                         |
    | 'коза съела капусту'                                 |    находиться(фермер, на, Y),         |                         |
    |                                                      |    берег(река, X),                    |                         |
    |                                                      |    берег(река, Y),                    |                         |
    |                                                      |    X \= Y,                            |                         |
    |                                                      |    Z = неудача,                       |                         |
    |                                                      |    причина(Z) = 'коза съела капусту'. |                         |
    |------------------------------------------------------|---------------------------------------|-------------------------|
    And the following sentences should fail to be understood
    | Sentence                                                 |
    |----------------------------------------------------------|
    | Если лебедь и рак отражаются на одной поверхности сферы, |
    | а щука отражается на другой поверхности сферы, то        |
    | басня начинается молчанием по причине                    |
    | 'не так сели'                                            |
    |----------------------------------------------------------|

Scenario: В начальном состоянии фермер, волк, коза и капуста находятся на левом береге реки
    Given the following Patterns
    | Id | Pattern                                                    | Meaning                               |
    |----|------------------------------------------------------------|---------------------------------------|
    | 6  | крестьянин{и проч.} покоится{мн.} на внешней поверхности   | покоиться(крестьянин, на, X),         |
    |    | кубика                                                     | поверхность(кубик, X),                |
    |    |                                                            | X = внешний                           |
    |----|------------------------------------------------------------|---------------------------------------|
    | 7  | ~В ∥(начальном, исходном) состоянии~ 'state-description'   | @call(                                |
    |    |                                                            |   @variable(∥, состояние),            |
    |    |                                                            |   [@understand('state-description')]).|
    |----|------------------------------------------------------------|---------------------------------------|
    Then the following understandings should be possible
    | Sentence                                           | Meaning                         | Recognized with Pattern |
    |----------------------------------------------------|---------------------------------|-------------------------|
    | В исходном состоянии 'фермер, волк, коза и капуста |  исходныйСостояние([            |       7-исходном        |
    | находятся на левом береге реки'                    |     находиться(фермер, на, X),  |                         |
    |                                                    |     находиться(волк, на, X),    |                         |
    |                                                    |     находиться(коза, на, X),    |                         |
    |                                                    |     находиться(капуста, на, X), |                         |
    |                                                    |     берег(река, X),             |                         |
    |                                                    |     X = левый]).                |                         |
    |----------------------------------------------------|---------------------------------|-------------------------|
    And the following sentences should fail to be understood
    | Sentence                                                                            |
    | В стартовом состоянии 'фермер, волк, коза и капуста находятся на левом береге реки' |


Scenario: Синтаксический разбор предложений с одинаковой структурой может давать по-разному устроенные графы
    Given the following Patterns
    | Id | Pattern                                                | Meaning                                                   |
    |----|--------------------------------------------------------|-----------------------------------------------------------|
    | 1  | электрон перемещается с ~одного~ уровня заряда         | перемещаться(электрон).                                   |
    |    | на ~другой~                                            |                                                           |
    |----|--------------------------------------------------------|-----------------------------------------------------------|
    | 2  | медведь перекатывается с ~одной~ стороны переулка      | перекатываться(медведь).                                  |
    |    | на ~другую~                                            |                                                           |
    |----|--------------------------------------------------------|-----------------------------------------------------------|
    | 3  | фермер переправляется с одного берега реки на другой   | переправляться(фермер).                                   |
    |----|--------------------------------------------------------|-----------------------------------------------------------|

Scenario: Применение мета-модификаторов к Meaning
    Given the following Patterns
    | Id  | Pattern                                               | Meaning                                                   |
    | ----| ------------------------------------------------------| ----------------------------------------------------------|
    | 2   | медведь лежит на ∥(одной, другой) стороне улицы       | лежать(медведь, @variable(∥, сторона)),                   |
    |     |                                                       | сторона(улица, @variable(∥, сторона))                     |
    | ----| ------------------------------------------------------| ----------------------------------------------------------|
    | 3   | заяц перепрыгивает с одного края бревна               | перепрыгивать(заяц, ОдинКрай, ДругойКрай),                |
    |     | на другой край бревна                                 | край(бревно, ОдинКрай),                                   |
    |     |                                                       | край(бревно, ДругойКрай),                                 |
    |     |                                                       | ОдинКрай \= ДругойКрай                                    |
    | ----| ------------------------------------------------------| ----------------------------------------------------------|
    | 4   | ~Воздействие~ 'impact' ~переводит систему из          | переводить(                                               |
    |     | состояния~ 'state-before' ~в состояние~ 'state-after' |       состояние([@understand('state-before')]),           |
    |     |                                                       |       воздействие(@understand('impact')),                 |
    |     |                                                       |       состояние([@understand('state-after')])).           |
    | ----| ------------------------------------------------------| ----------------------------------------------------------|
    Then the following understandings should be possible
    | Sentence                                                    | Meaning                                                          |
    |-------------------------------------------------------------|------------------------------------------------------------------|
    |  Воздействие 'фермер переправляется с одного берега реки    | переводить(                                                      |
    |  на другой берег реки' переводит систему из состояния       |     состояние([находиться(фермер, ОдинБерег),                    |
    |  'фермер находится на одном береге реки' в состояние        |                берег(река, ОдинБерег)]),                         |
    |  'фермер находится на другом береге реки'                   |     воздействие(переправляться(фермер, ОдинБерег, ДругойБерег),  |
    |                                                             |                 берег(река, ОдинБерег),                          |
    |                                                             |                 берег(река, ДругойБерег),                        |
    |                                                             |                 ОдинБерег \= ДругойБерег),                       |
    |                                                             |     состояние([находиться(фермер, ДругойБерег),                  |
    |                                                             |                берег(река, ДругойБерег)])                        |
    |                                                             | ).                                                               |
    |-------------------------------------------------------------|------------------------------------------------------------------|

Scenario: В случае паттернов вида [~что-то фиксированное~ 'некая quote' ~еще что-то фиксированное~ 'другая quote'] мы можем не использовать кавычки для обозначения quotes
    Given the following Patterns
    | Id | Pattern                                               | Meaning                                          |
    |----|-------------------------------------------------------|--------------------------------------------------|
    | 1  | ~В ∥(начальном, исходном) состоянии~                  | @call(                                           |
    |    |        'state-description'                            |   @variable(∥, состояние),                       |
    |    |                                                       |   [@understand('state-description')]).           |
    |----|-------------------------------------------------------|--------------------------------------------------|
    | 2  | ~Воздействие~ 'impact' ~переводит систему из          | переводить(                                      |
    |    | состояния~ 'state-before' ~в состояние~ 'state-after' |       состояние([@understand('state-before')]),  |
    |    |                                                       |       воздействие(@understand('impact')),        |
    |    |                                                       |       состояние([@understand('state-after')])).  |
    |----|-------------------------------------------------------|--------------------------------------------------|
    | 3  | ботинок{и проч.} ~находится{мн.}~ на ∥(одной, другой) | находиться(ботинок, @variable(∥, сторона)),      |
    |    | стороне лужи                                          | сторона(лужа, @variable(∥, сторона))             |
    |----| ------------------------------------------------------| -------------------------------------------------|
    | 5  | заяц перепрыгивает с ~одного~ края бревна             | перепрыгивать(заяц, ОдинКрай, ДругойКрай),       |
    |    | на ~другой~ край бревна                               | край(бревно, ОдинКрай),                          |
    |    |                                                       | край(бревно, ДругойКрай),                        |
    |    |                                                       | ОдинКрай \= ДругойКрай                           |
    |----| ------------------------------------------------------| -------------------------------------------------|
    Then the following understandings should be possible
    | Sentence                                          | Meaning                                                          |
    |---------------------------------------------------|------------------------------------------------------------------|
    | В исходном состоянии фермер, волк, коза и капуста |  исходныйСостояние([                                             |
    | находятся на левом береге реки                    |     находиться(фермер, ЛевыйБерег),                              |
    |                                                   |     находиться(волк, ЛевыйБерег),                                |
    |                                                   |     находиться(коза, ЛевыйБерег),                                |
    |                                                   |     находиться(капуста, ЛевыйБерег),                             |
    |                                                   |     берег(река, ЛевыйБерег)                                      |
    |                                                   |  ]).                                                             |
    |---------------------------------------------------|------------------------------------------------------------------|
    |  Воздействие фермер переправляется с одного       | переводить(                                                      |
    |  берега реки на другой берег реки переводит       |     состояние([находиться(фермер, ОдинБерег),                    |
    |  систему из состояния фермер находится            |                берег(река, ОдинБерег)]),                         |
    |  на одном береге реки в состояние фермер          |     воздействие(переправляться(фермер, ОдинБерег, ДругойБерег),  |
    |  находится на другом береге реки                  |                 берег(река, ОдинБерег),                          |
    |                                                   |                 берег(река, ДругойБерег),                        |
    |                                                   |                 ОдинБерег \= ДругойБерег),                       |
    |                                                   |     состояние([находиться(фермер, ДругойБерег),                  |
    |                                                   |                берег(река, ДругойБерег)])                        |
    |                                                   | ).                                                               |
    |---------------------------------------------------|------------------------------------------------------------------|


Scenario: Meaning часть паттерна может быть формальным высказыванием на естественном языке
    Meaning часть паттерна может содержать не код на Prolog, а так же высказывание на естественном языке, 
    которое может быть распознано другим имеющимся паттерном
    Given the following Patterns
    | Id  | Pattern                                         | Meaning                                            |
    | ----| ------------------------------------------------| ---------------------------------------------------|
    | 0   | Множество м состоит из следующих элементов:     | множество(м, [первый, второй]).                    |
    |     |   первый, второй                                |                                                    |
    | ----| ------------------------------------------------| ---------------------------------------------------|
    | 1   | Сфера ~имеет~ внешнюю и внутреннюю поверхности  | @understand(% множество поверхностьСфера состоит   |
    |     |                                                 | из следующих элементов: внешний, внутренний %)     |
    | ----| ------------------------------------------------| ---------------------------------------------------|
    Then the following understandings should be possible
    | Sentence                                              | Meaning                                            |
    | река имеет левый и правый берега                      | множество(берегРека, [левый, правый]).             |
