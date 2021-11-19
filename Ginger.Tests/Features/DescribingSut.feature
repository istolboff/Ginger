Feature: Как фразы, описывающие SUT, транслируются в Prolog-факты и правила


Background: 
    Given the following Patterns
    | Id  | Pattern                                               | Meaning                                                   |
    | ----| ------------------------------------------------------| ----------------------------------------------------------|
    | 0   | Множество М состоит из следующих элементов:           | множество(М, [голубой]).                                  |
    |     |   голубой {и проч.}                                   |                                                           |
    | ----| ------------------------------------------------------| ----------------------------------------------------------|
    | 1   | Сфера ~имеет~ внешнюю и внутреннюю поверхности        | множество 'поверхностьСферы' состоит из следующих         |
    |     |                                                       | элементов: внешний, внутренний                            |
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
    | 5a  | Если медведь{и проч.} располагается{мн.} на верхнем   | завершаться(приключение, взрыв) :-                        |
    |     | конце острова, то приключение завершается взрывом     |     располагаться(медведь, X),                            |
    |     |                                                       |     конец(остров, X),                                     |
    |     |                                                       |     X = верхний.                                          |
    | ----| ------------------------------------------------------| ----------------------------------------------------------|
    | 5b  | Если медведь{и проч.} располагается{мн.} на ~одном~   | завершаться(история, катастрофа, диагноз(апперцепция)) :- |
    |     | конце бассейна, а заяц располагается на ~другом~ конце |     располагаться(медведь, ОдинКонец),                    |
    |     | бассейна, то история завершается катастрофой с        |     располагаться(заяц, ДругойКонец),                     |
    |     | диагнозом 'апперцепция'                               |     конец(бассейн, ОдинКонец),                            |
    |     |                                                       |     конец(бассейн, ДругойКонец),                          |
    |     |                                                       |     ОдинКонец \= ДругойКонец.                             |
    |-----| ------------------------------------------------------| ----------------------------------------------------------|


Scenario: Как преобразуются Prolog-правила, описывающие воздействия на SUT
    В правилах, описывающих воздействия на SUT, должен осуществляться следующий рефакторинг: 
    если в Conclusion-части этих правил встречается complex term, который либо структурно совпадает с Prolog-фактом, 
    описывающим какую-нибудь сущность в SUT, либо являются Built-in предикатом типа =, \=, 
    то этот complex term должен быть перемещен из Conclusion-части в Premises этого правила.

    Given SUT is described as follows
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

Scenario: Как преобразуется код Prolog, соответствующий описанию Правил поведения SUT
    Правила поведения SUT (все они иеют вид Conclusion :- Premise1, Premise2,..., PremiseN) преобразуются 
    в следующий вид:
        конечноеСостояние(Conclusion, [PremiseX, PremiseY, ..., PremiseZ]) :- PremiseA, PremiseB, ..., PremiseC
    где PremiseA, PremiseB, ..., PremiseC -- это те условия из списка Premise1, Premise2,..., PremiseN, которые 
        либо структурно совпадают с Prolog-фактом, описывающим какую-нибудь сущность в SUT, либо являются Built-in 
        предикатом типа =, \=;
    и где PremiseX, PremiseY, ..., PremiseZ -- все остальные условия из списка Premise1, Premise2,..., PremiseN.

    Given SUT is described as follows
    | Type              | Phrasing                                                                                   |
    |-------------------|--------------------------------------------------------------------------------------------|
    | Сущность          | Река имеет левый и правый берега                                                           |
    |-------------------|--------------------------------------------------------------------------------------------|
    | Сущность          | волк, коза и капуста являются перевозимыми существами                                      |
    |-------------------|--------------------------------------------------------------------------------------------|
    | Правило поведения | Если волк, коза и капуста находятся на правом береге реки, то миссия заканчивается успехом |
    |-------------------|--------------------------------------------------------------------------------------------|
    | Правило поведения | Если волк и коза находятся на одном береге реки, а фермер находится на другом береге реки, |
    |                   | то миссия заканчивается неудачей с формулировкой 'волк съел козу'                          |
    |-------------------|--------------------------------------------------------------------------------------------|
    | Правило поведения | Если коза и капуста находятся на одном береге реки, а фермер находится на другом береге    |
    |                   | реки, то миссия заканчивается неудачей с формулировкой 'коза съела капусту'                |
    |-------------------|--------------------------------------------------------------------------------------------|
    Then the following components of SUT description should be generated
    | Type              | Prolog Rules                                                            |
    |-------------------|-------------------------------------------------------------------------|
    | Правило поведения | конечноеСостояние(                                                      |
    |                   |     заканчиваться(миссия, 'успех'),                                     |
    |                   |     [находиться(волк, X), находиться(коза, X), находиться(капуста, X)]) |
    |                   | :-                                                                      |
    |                   |       	берег(река, X),                                               |
    |                   |        	X = правый.                                                   |
    |-------------------|-------------------------------------------------------------------------|
    | Правило поведения | конечноеСостояние(                                                      |
    |                   |     заканчиваться(миссия, неудача, формулировка('волк съел козу')),     |
    |                   |     [находиться(волк, ОдинБерег),                                       |
    |                   |      находиться(коза, ОдинБерег),                                       |
    |                   |      находиться(фермер, ДругойБерег)])                                  |
    |                   | :-                                                                      |
    |                   |           берег(река, ОдинБерег),                                       |
    |                   |           берег(река, ДругойБерег),                                     |
    |                   |           ОдинБерег \= ДругойБерег.                                     |
    |-------------------|-------------------------------------------------------------------------|
    | Правило поведения | конечноеСостояние(                                                      |
    |                   |     заканчиваться(миссия, неудача, формулировка('коза съела капусту')), |
    |                   |     [находиться(коза, ОдинБерег),                                       |
    |                   |      находиться(капуста, ОдинБерег),                                    |
    |                   |      находиться(фермер, ДругойБерег)])                                  |
    |                   | :-                                                                      |
    |                   |           берег(река, ОдинБерег),                                       |
    |                   |           берег(река, ДругойБерег),                                     |
    |                   |           ОдинБерег \= ДругойБерег.                                     |
    |-------------------|-------------------------------------------------------------------------|
