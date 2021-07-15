Feature: Как фразы, описывающие SUT, транслируются в Prolog-факты и правила

Scenario: Как преобразуются Prolog-правила, описывающие воздействия на SUT
    В правилах, описывающих воздействия на SUT, должен осуществляться следующий рефакторинг: 
    если в Conclusion-части этих правил встречается complex term, который структурно совпадает с Prolog-фактом, 
    описывающим какую-нибудь сущность в SUT, то этот complex term должен быть перемещен из Conclusion-части в 
    Premises этого правила.

    Given the following Patterns
    | Id  | Pattern                                               | Meaning                                                   |
    | ----| ------------------------------------------------------| ----------------------------------------------------------|
    | 1   | Сфера ~имеет~ внешнюю и внутреннюю поверхности        | поверхность(сфера, внешний).                              |
    |     |                                                       | поверхность(сфера, внутренний).                           |
    | ----| ------------------------------------------------------| ----------------------------------------------------------|
    | 1+  | медведь{и проч.} является{мн.} отрицательным{мн.}     | отрицательныйГерой(медведь).                              |
    |     | героем{мн.}                                           |                                                           |
    |-----|-------------------------------------------------------|-----------------------------------------------------------|
    | 2   | медведь лежит на ∥(одной, другой) стороне улицы       | лежать(медведь, @variable(∥, сторона)),                   |
    |     |                                                       | сторона(улица, @variable(∥, сторона))                     |
    | ----| ------------------------------------------------------| ----------------------------------------------------------|
    | 2+  | медведь и лесной житель лежат на ∥(одной, другой)     | лежать(медведь, @variable(∥, сторона)),                   |
    |     | стороне улицы                                         | лежать(X, @variable(∥, сторона)),                         |
    |     |                                                       | леснойЖитель(X),                                          |
    |     |                                                       | сторона(улица, @variable(∥, сторона))                     |
    | ----| ------------------------------------------------------| ----------------------------------------------------------|
    | 3.1 | заяц перепрыгивает с ~одного~ края бревна             | перепрыгивать(заяц, ОдинКрай, ДругойКрай),                |
    |     | на ~другой~ край бревна                               | край(бревно, ОдинКрай),                                   |
    |     |                                                       | край(бревно, ДругойКрай),                                 |
    |     |                                                       | ОдинКрай \= ДругойКрай                                    |
    | ----| ------------------------------------------------------| ----------------------------------------------------------|
    | 3.2 | заяц перекатывает переносимое имущество с ~одного~    | перекатывать(заяц, X, ОдинКонец, ДругойКонец),            |
    |     | конца бревна на ~другой~ конец бревна                 | переносимыйИмущество(X),                                  |
    |     |                                                       | конец(бревно, ОдинКонец),                                 |
    |     |                                                       | конец(бревно, ДругойКонец),                               |
    |     |                                                       | ОдинКонец \= ДругойКонец                                  |
    | ----| ------------------------------------------------------| ----------------------------------------------------------|
    | 4   | ~Воздействие~ 'impact' ~переводит систему из          | переводить(                                               |
    |     | состояния~ 'state-before' ~в состояние~ 'state-after' |       состояние([@understand('state-before')]),           |
    |     |                                                       |       воздействие(@understand('impact')),                 |
    |     |                                                       |       состояние([@understand('state-after')])).           |
    | ----| ------------------------------------------------------| ----------------------------------------------------------|
    And SUT is described as follows
    | Type              | Phrasing                                                  |
    |-------------------|-----------------------------------------------------------|
    | Сущность          | Река имеет левый и правый берега                          |
    |-------------------|-----------------------------------------------------------|
    | Сущность          | волк, коза и капуста являются перевозимыми существами     |
    |-------------------|-----------------------------------------------------------|
    | Воздействие       |  Воздействие 'фермер переправляется с одного берега реки  |
    |                   |  на другой берег реки' переводит систему из состояния     |
    |                   |  'фермер находится на одном береге реки' в состояние      |
    |                   |  'фермер находится на другом береге реки'                 |
    |-------------------|-----------------------------------------------------------|
    | Воздействие       |  Воздействие 'фермер перевозит перевозимое существо с     |
    |                   |  одного берега реки на другой берег реки' переводит       |
    |                   |  систему из состояния 'фермер и перевозимое существо      |
    |                   |  находятся на одном береге реки' в состояние 'фермер и    |
    |                   |  перевозимое существо находятся на другом береге реки'    |
    |-------------------|-----------------------------------------------------------|
    Then the following components of SUT description should be generated
    | Type              | Prolog Rules                                                      |
    |-------------------|-------------------------------------------------------------------|
    | Воздействие       |  переводить(                                                      |      
    |                   |     состояние([находиться(фермер, ОдинБерег)]),                   |
    |                   |     воздействие(переправляться(фермер, ОдинБерег, ДругойБерег)),  |
    |                   |     состояние([находиться(фермер, ДругойБерег)]))                 |
    |                   |   :-                                                              |
    |                   |          берег(река, ОдинБерег),                                  |
    |                   |          берег(река, ДругойБерег),                                |
    |                   |          ОдинБерег \= ДругойБерег.                                |                     
    |-------------------|-------------------------------------------------------------------|
    | Воздействие       |  переводить(                                                      |      
    |                   |     состояние([находиться(фермер, ОдинБерег),                     |
    |                   |                находиться(X, ОдинБерег)]),                        |
    |                   |     воздействие(перевозить(фермер, X, ОдинБерег, ДругойБерег)),   |
    |                   |     состояние([находиться(фермер, ДругойБерег),                   |
    |                   |                находиться(X, ДругойБерег)]))                      |
    |                   |   :-                                                              |
    |                   |          перевозимыйСущество(X),                                  |
    |                   |          берег(река, ОдинБерег),                                  |
    |                   |          берег(река, ДругойБерег),                                |
    |                   |          ОдинБерег \= ДругойБерег.                                |                     
    |-------------------|-------------------------------------------------------------------|