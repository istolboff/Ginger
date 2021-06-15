Feature: We generate testing scenarios from the formal description of the System Under Test

@Ignore
Scenario: Wolf-Goat-Cabbage riddle solving
   Given SUT is described as follows
    | Type              | Phrasing                                                                                      |
    |-------------------|-----------------------------------------------------------------------------------------------|
    | Сущность          | Река имеет левый и правый берега                                                              |
    |-------------------|-----------------------------------------------------------------------------------------------|
    | Сущность          | Волк, коза и капуста являются перевозимыми существами                                         |
    |-------------------|-----------------------------------------------------------------------------------------------|
    | Воздействие       |  Воздействие 'фермер перевозит перевозимое существо с одного берега реки на другой'           |
    |                   |  переводит систему из состояния 'фермер и перевозимое существо находятся на одном береге' в   |
    |                   |  состояние 'фермер и перевозимое существо находятся на другом береге'                         |
    |-------------------|-----------------------------------------------------------------------------------------------|
    | Воздействие       |  Воздействие 'фермер переправляется с одного берега на другой берег' переводит систему из     |
    |                   |  состояния 'фермер находится на одном береге' в                                               |
    |                   |  состояние 'фермер находится на другом береге'                                                |
    |-------------------|-----------------------------------------------------------------------------------------------|
    | Начальное условие | В начальном состоянии фермер, волк, коза и капуста находятся на левом береге реки             |
    |-------------------|-----------------------------------------------------------------------------------------------|
    | Правило           | Если волк, коза и капуста находятся на правом береге реки, то миссия заканчивается успехом    |
    |-------------------|-----------------------------------------------------------------------------------------------|
    | Правило           | Если волк и коза находятся на одном береге реки, а фермер находится на другом береге реки,    |
    |                   | то миссия заканчивается неудачей с формулировкой 'волк съел козу'                             |
    |-------------------|-----------------------------------------------------------------------------------------------|
    | Правило           | Если коза и капуста находятся на одном береге реки, а фермер находится на другом береге реки, |
    |                   | то миссия заканчивается неудачей с формулировкой 'коза съела капусту'                         |
    |-------------------|-----------------------------------------------------------------------------------------------|
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