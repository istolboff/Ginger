Feature: We generate testing scenarios from the formal description of the System Under Test

Background: 
   Given the following Patterns
    | Id  | Pattern                                               | Meaning                                                   |
    | ----| ------------------------------------------------------| ----------------------------------------------------------|
    | 1   | Сфера ~имеет~ внешнюю и внутреннюю поверхности        | поверхность(сфера, внешний).                              |
    |     |                                                       | поверхность(сфера, внутренний).                           |
    | ----| ------------------------------------------------------| ----------------------------------------------------------|
    | 2   | медведь{и проч.} является{мн.} отрицательным{мн.}     | отрицательныйГерой(медведь).                              |
    |     | героем{мн.}                                           |                                                           |
    | ----| ------------------------------------------------------| ----------------------------------------------------------|
    | 3   | ~Воздействие~ 'impact' ~переводит систему из          | переводить(                                               |
    |     | состояния~ 'state-before' ~в состояние~ 'state-after' |       состояние([@understand('state-before')]),           |
    |     |                                                       |       воздействие(@understand('impact')),                 |
    |     |                                                       |       состояние([@understand('state-after')])).           |
    | ----| ------------------------------------------------------| ----------------------------------------------------------|
    | 4   | заяц перекатывает переносимое имущество с ~одного~    | перекатывать(заяц, X, ОдинКонец, ДругойКонец),            |
    |     | конца бревна на ~другой~ конец бревна                 | переносимыйИмущество(X),                                  |
    |     |                                                       | конец(бревно, ОдинКонец),                                 |
    |     |                                                       | конец(бревно, ДругойКонец),                               |
    |     |                                                       | ОдинКонец \= ДругойКонец                                  |
    | ----| ------------------------------------------------------| ----------------------------------------------------------|
    | 5   | заяц перепрыгивает с ~одного~ края бревна             | перепрыгивать(заяц, ОдинКрай, ДругойКрай),                |
    |     | на ~другой~ край бревна                               | край(бревно, ОдинКрай),                                   |
    |     |                                                       | край(бревно, ДругойКрай),                                 |
    |     |                                                       | ОдинКрай \= ДругойКрай                                    |
    | ----| ------------------------------------------------------| ----------------------------------------------------------|
    | 6   | ботинок{и проч.} ~находится{мн.}~ на ∥(одной, другой) | находиться(ботинок, @variable(∥, сторона)),               |
    |     | стороне лужи                                          | сторона(лужа, @variable(∥, сторона))                      |
    | ----| ------------------------------------------------------| ----------------------------------------------------------|
    | 7   | медведь лежит на ∥(одной, другой) стороне улицы       | лежать(медведь, @variable(∥, сторона)),                   |
    |     |                                                       | сторона(улица, @variable(∥, сторона))                     |
    | ----| ------------------------------------------------------| ----------------------------------------------------------|
    | 8   | ~В ∥(начальном, исходном) состоянии~                  | @call(                                                    |
    |     |        'state-description'                            |   @variable(∥, состояние),                                |
    |     |                                                       |   [@understand('state-description')]).                    |
    |-----|-------------------------------------------------------|-----------------------------------------------------------|
    | 9   | ~если~ 'условие', ~то~ 'cледствие'                    | @understand('cледствие') :-                               |
    |     |                                                       |                    @understand('условие').                |
    |-----|-------------------------------------------------------|-----------------------------------------------------------|


Scenario: Wolf-Goat-Cabbage riddle solving
   Given SUT is described as follows
    | Type                | Phrasing                                                                                      |
    |---------------------|-----------------------------------------------------------------------------------------------|
    | Сущность            | Река имеет левый и правый берега                                                              |
    |---------------------|-----------------------------------------------------------------------------------------------|
    | Сущность            | Волк, коза и капуста являются перевозимыми существами                                         |
    |---------------------|-----------------------------------------------------------------------------------------------|
    | Воздействие         | Воздействие 'фермер перевозит перевозимое существо с одного берега реки на другой берег реки' |
    |                     | переводит систему из состояния 'фермер и перевозимое существо находятся на одном береге реки' |
    |                     | в состояние 'фермер и перевозимое существо находятся на другом береге реки'                   |
    |---------------------|-----------------------------------------------------------------------------------------------|
    | Воздействие         |  Воздействие 'фермер переправляется с одного берега реки на другой берег реки' переводит      |
    |                     |  систему из состояния 'фермер находится на одном береге реки' в                               |
    |                     |  состояние 'фермер находится на другом береге реки'                                           |
    |---------------------|-----------------------------------------------------------------------------------------------|
    | Начальное состояние | В начальном состоянии фермер, волк, коза и капуста находятся на левом береге реки             |
    |---------------------|-----------------------------------------------------------------------------------------------|
    | Правило поведения   | Если волк, коза и капуста находятся на правом береге реки, то миссия заканчивается успехом    |
    |---------------------|-----------------------------------------------------------------------------------------------|
    | Правило поведения   | Если волк и коза находятся на одном береге реки, а фермер находится на другом береге реки,    |
    |                     | то миссия заканчивается неудачей с формулировкой 'волк съел козу'                             |
    |---------------------|-----------------------------------------------------------------------------------------------|
    | Правило поведения   | Если коза и капуста находятся на одном береге реки, а фермер находится на другом береге реки, |
    |                     | то миссия заканчивается неудачей с формулировкой 'коза съела капусту'                         |
    |---------------------|-----------------------------------------------------------------------------------------------|
    Then the following scenarios should be generated
    | Expected Outcome             | Scenario Steps                                    |
    |------------------------------|---------------------------------------------------|
    | миссия заканчивается успехом | [ % Единственный способ                           |
    |                              |   [перевозит(фермер, коза, левый, правый),        |
    |                              |    переправляется(фермер, правый, левый),         |
    |                              |    перевозит(фермер, волк, левый, правый),        |
    |                              |    перевозит(фермер, коза, правый, левый),        |
    |                              |    перевозит(фермер, капуста, левый, правый),     |
    |                              |    переправляется(фермер, правый, левый),         | 
    |                              |    перевозит(фермер, коза, левый, правый)]        |
    |                              |  ]                                                |
    |------------------------------|---------------------------------------------------|
    | волк съел козу               | [  % Первый способ                                |
    |                              |    [перевозит(фермер, капуста, левый, правый)],   |
    |                              |    % Второй способ                                |
    |                              |    [переправляется(фермер, правый, левый)],       |
    |                              |    % Третий способ                                |
    |                              |    [                                              |
    |                              |       перевозит(фермер, коза, левый, правый),     |
    |                              |       переправляется(фермер, правый, левый),      |
    |                              |       перевозит(фермер, волк, левый, правый),     | 
    |                              |       переправляется(фермер, правый, левый)       |
    |                              |    ]                                              |
    |                              | ]                                                 |
    |------------------------------|---------------------------------------------------|
    | коза съела капусту           | [  % Первый способ                                |
    |                              |    [перевозит(фермер, волк, левый, правый)],      |
    |                              |    % Второй способ                                |
    |                              |    [                                              |
    |                              |       перевозит(фермер, коза, левый, правый),     |
    |                              |       переправляется(фермер, правый, левый),      |
    |                              |       перевозит(фермер, капуста, левый, правый),  |
    |                              |       переправляется(фермер, правый, левый)       |
    |                              |    ]                                              |
    |                              | ]                                                 |
    |------------------------------|---------------------------------------------------|
